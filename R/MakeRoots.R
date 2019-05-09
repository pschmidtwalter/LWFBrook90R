#' Generates a root density depth function the soil layers' lower depth limits
#'
#' @param soilnodes vector of lower soil layer depth limits,
#' for which the relative root distribution will be calculated.
#' @param maxrootdepth the maximum rooting depth (m, negative downwards) below which
#' relative root length density will be set to zero
#' @param method method name for the root depth distribution. 'betamodel' uses the model after Gale & Grigal,
#' 'table' interpolates the value pairs of 'rootdepths' and 'relrootden' to 'soilnodes'.
#' 'linear' returns linearly decreasing root densities with the maximum value
#' (taken from the first vector element of 'relrootden') at the uppermost layer to 0
#' at 'maxrootdepth'. 'constant' returns a uniform root distribution with depth
#' corresponding to the first vector element of 'relrootden' and 0 below 'maxrootdepth'.
#' @param beta parameter(s) of the root distribution function
#' @param relrootden vector of relative root densities
#' @param rootdepths vector of lower depths limit, corresponding to 'relrootden'
#'
#' @return vector of relative rootlength, corresponding to soilnodes
#' @examples
#'
#' depths <- slb1_soil$lower
#' roots_beta <- MakeRelRootDens(soilnodes = depths,
#'                               maxrootdepth = -1,4,
#'                               beta = 0.97,
#'                               method = "betamodel")
#'
#' rootden.table <- data.frame(
#'   depth = c(-0.02, -0.15, -0.35, -0.5, -0.65,-0.9,-1.1,-1.3,-1.6),
#'   rootden = c(15, 35, 15, 7.5, 4, 12, 2, 2, 0))
#'
#' roots_table <- MakeRelRootDens(soilnodes = depths,
#'                                method = "table",
#'                                relrootden = rootden.table$rootden,
#'                                rootdepths = rootden.table$depth)
#'
#' roots_linear <- MakeRelRootDens(soilnodes = depths,
#'                                 maxrootdepth = -1.4,
#'                                 method = 'linear',
#'                                 relrootden = 0.2)
#'
#' roots_constant <- MakeRelRootDens(soilnodes = depths,
#'                                   maxrootdepth = -1.4,
#'                                   method = 'const',
#'                                   relrootden = 0.2)
#'
#' plot(roots_constant, slb1_soil$lower +runif(n=length(slb1_soil$lower), -0.02,0.02),
#'      type = 's', lwd = 1.5,ylab = "soil depth [m]",xlab = "relative root density",
#'      xlim = c(0,0.35), col = "red")
#' lines(roots_linear, slb1_soil$lower,
#'       type = 's', col = "blue", lwd = 1.5)
#' lines(roots_table/100, slb1_soil$lower+runif(n=length(slb1_soil$lower), -0.02,0.02),
#'       type = 's', col = "green", lwd = 1.5)
#' lines(roots_beta, slb1_soil$lower, type = 's', col = "brown", lwd = 1.5)
#'
#' legend("bottomright", c("'betamodel'","'table'","'linear'", "'constant'"),seg.len = 1.5,
#'        pch = NULL, lwd =1.5, col = c("brown", "green", "blue", "red"), bty = "n")



#' @export
MakeRelRootDens <- function(soilnodes,
                            maxrootdepth = min(soilnodes),
                            method = "betamodel",
                            beta = 0.97, #
                            relrootden = NULL,
                            rootdepths=NULL
                            #cum_RLenDmax= 0.95 # maximum cumulative rootlength, at which maximim
) {
  method <- match.arg(method, choices = c("betamodel", "table", "constant", "linear"))

  if (method == "betamodel") {
    # only positive d-values allowed in beta-model:
    #maxrootdepth <- soilnodes[which(abs(soilnodes - maxrootdepth) == min(abs(soilnodes-maxrootdepth)))]
    maxrootdepth <- maxrootdepth * (-100)
    soilnodes <- soilnodes * (-100)

    # cumulative density
    # shift min(soilnodes) to 1 and extend maxrootdepth
    RLenDcum <- 1 - (beta ^ seq(1,maxrootdepth-(min(soilnodes)-1)))
    # density
    RLenD <- c(RLenDcum[1], diff(RLenDcum))

    # linear approx function is derived using the "unshifted" values
    RelDenFun <- approxfun(x = seq(min(soilnodes),maxrootdepth),
                           y = RLenD,
                           method = "linear",
                           rule = 2:1, yright = 0) # rule 2:1: left > max(x) -> repeat, right = 0

    # get midpoints of the soilnodes:
    midpoints <- c(min(soilnodes) - 1, soilnodes[1:length(soilnodes)-1]) + (diff(c(min(soilnodes) - 1,soilnodes))/2)
    # calc the rootden for the midpoints to be exact, and normalize to unity
    rootden <- RelDenFun(midpoints) * (1/sum(RelDenFun(midpoints)))
  }

  if (method == "table") {
    RelDenFun <- approxfun(x = rootdepths, y = relrootden,
                                              method = "linear", rule = 1:2,  yleft = 0)
    midpoints <- c(min(soilnodes) + 0.01, soilnodes[1:length(soilnodes)-1]) + (diff(c(min(soilnodes) +0.01, soilnodes))/2)
    rootden <- RelDenFun(midpoints)
  }

  if (method == "constant") {
    if (is.null(relrootden)) {relrootden <- 1}
    RelDenFun <- approxfun(x = c(max(soilnodes),maxrootdepth), y = c(relrootden[1],relrootden[1]),
                           method = "constant",rule = 1:2, yleft = 0)
    rootden <- RelDenFun(soilnodes)
  }

  if (method == "linear") {
    if (is.null(relrootden)) {relrootden <- 1}
    RelDenFun <- approxfun(x = c(max(soilnodes),maxrootdepth), y = c(relrootden[1],0), method = "linear",rule = 1:2, yleft = 0)
    midpoints <- c(max(soilnodes) + 0.01, soilnodes[1:length(soilnodes)-1]) + (diff(c(max(soilnodes) +0.01, soilnodes))/2)
    rootden <- RelDenFun(midpoints)
  }
  return(rootden)
}


# beta <- optimize(function(beta,z,maxroot.pct){abs(maxroot.pct- (1 - (beta^z)))},
#                  c(0,1), z=85, maxroot.pct=0.95 )$minimum
# beta <- 0.99
# RLenD <- 1 - (beta^seq(1,120))
# #wurzeln anf?gen
# RLenD <- c(RLenD[1], diff(RLenD))
#
# soil$relrootlength <- round(approx(x = seq(1,length(RLenD)),y = RLenD, xout=soil$Lower*-100,method = "linear",rule = 2:1, yright = 0)$y,4)

