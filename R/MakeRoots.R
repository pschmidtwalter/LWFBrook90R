#' Generates a root density depth function for soil layers
#'
#' @param soilnodes Vector of soil layer depth limits (including the top and the bottom of the profile)
#' for which the relative root distribution will be calculated (m, negative downwards).
#' @param maxrootdepth The maximum rooting depth (m, negative downwards) below which
#' relative root length density will be set to zero (not applying when \code{method = 'table'}).
#' @param method Method name for the root depth distribution. Possible values are 'betamodel',
#' 'table', 'linear', 'constant'. See details.
#' @param beta Parameter of the root distribution function.
#' @param rootdat data.frame with a given root depth density distribution. Columns are depth limits
#' ('upper' and 'lower' in m, negative downwards) and relative root densities of fine or absorbing roots
#' ('rootden') per unit stonefree volume. Only used when \code{method = 'table'}.
#'
#' @return Vector of relative root length densities for the soil layers framed by \code{soilnodes}.
#' Length is one less than \code{length(soilnodes)}.
#'
#' @details \code{method = 'betamodel'} calculates the relative root length densities of the soil layers
#' from the cumulative proportion of roots derived by the model after Gale & Grigal (1987).
#' \code{method = 'table'} distributes the relative root densities provided by \code{rootdat} to the soil layers,
#' under preservation of total root mass. \code{method = 'linear'} returns linearly decreasing root densities
#' with a value of 1 at the top of the soil profile to 0 at \code{maxrootdepth}.
#' \code{method = 'constant'} returns a uniform root distribution with a relative root length density of 1
#' for all soil layers above \code{'maxrootdepth'}.
#'
#' @references
#' Gale, M.R. & Grigal D.F. (1987): "Vertical root distributions of northern tree
#' species in relation to successional status."
#' \emph{Canadian Journal of Forest Research}, \emph{17:829-834}
#'
#' @example inst/examples/MakeRelRootDens-help.R
#' @export
MakeRelRootDens <- function(soilnodes,
                            maxrootdepth = min(soilnodes),
                            method = "betamodel",
                            beta = 0.97,
                            rootdat = NULL
) {
  method <- match.arg(method, choices = c("betamodel", "table", "constant", "linear"))

  if (method == "betamodel") {

    # only positive d-values allowed in beta-model:
    maxrootdepth <- maxrootdepth * (-100)
    soilnodes <- soilnodes * (-100)

    # replace first element greater maxrootdepth with maxrootdepth
    soilnodes_maxrtdep <- soilnodes
    soilnodes_maxrtdep[which.max(soilnodes >= maxrootdepth)] <- maxrootdepth

    # shift downwards to account for negative values in soilnodes (humus topsoil layers)
    if (min(soilnodes_maxrtdep) < 0) {
      maxrootdepth <- maxrootdepth - min(soilnodes_maxrtdep)
      soilnodes_maxrtdep <- soilnodes_maxrtdep - min(soilnodes_maxrtdep)
    }

    # cumulative density
    RLenDcum <- 1 - (beta ^ soilnodes_maxrtdep)

    # density
    rootden <- diff(RLenDcum)/diff(soilnodes) # important to use soilnodes here, so rootden is reduced if the lowest layer is only partially within maxrootdpeth
    rootden[which(soilnodes_maxrtdep > maxrootdepth) - 1] <- 0

  }

  if (method == "constant") {
    rootden <- rep(1,length(soilnodes)-1)
    #rootden[which(soilnodes[2:length(soilnodes)] < maxrootdepth)] <- 0
    rootden[which(soilnodes[1:length(soilnodes)-1] <= maxrootdepth)] <- 0
  }

  if (method == "linear") {
    RelDenFun <- stats::approxfun(x = c(max(soilnodes),maxrootdepth), y = c(1,0), method = "linear",rule = 1:2, yleft = 0)
    soilnodes[which.max(soilnodes <= maxrootdepth)] <- maxrootdepth
    midpoints <- soilnodes[1:length(soilnodes)-1] + diff(soilnodes)/2
    #rootden <- RelDenFun(soilnodes[2:length(soilnodes)])
    rootden <- RelDenFun(midpoints)
  }

  if (method == "table") {

    # distributes 'measured' relative root densities to the soil layers, preserving total root mass

    # create data.tables for overlap join
    rootdat <- data.table::data.table(rootdat, key = c("lower", "upper"))
    rootdat[, rthick := (upper - lower)]
    rootdat[,rootmass := rootden*rthick]

    slayers <- data.table::data.table(upper = soilnodes[1:length(soilnodes)-1],
                                      lower = soilnodes[2:length(soilnodes)])

    # overlap-join
    rootdat <- data.table::foverlaps(slayers,rootdat, type = "any")

    # derive overlap-thickness for soil layers
    rootdat[,thick_ol := (ifelse(i.upper < upper,i.upper,upper) -
                            ifelse(i.lower < lower & i.upper > lower,lower,
                                   ifelse(i.upper < lower,0,i.lower)) ) * (i.upper > lower & i.lower < upper)]

    # sum up rootmass proportional to overlapping thickness
    out <- rootdat[, list(i.rootmass = sum(rootmass*thick_ol/rthick)), by = list(i.upper ,i.lower)]

    # convert rootmass back to root density
    out[,i.rden := ifelse(!is.na(i.rootmass),i.rootmass / (i.upper - i.lower), 0)]

    rootden <- out$i.rden

  }
  return(rootden)
}
