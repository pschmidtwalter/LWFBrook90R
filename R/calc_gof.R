#' Calculate Goodness-of-fit for simulation result using observations
#'
#'
#' @param simres list of data.frames with simulation results
#' @param obs data.frame of observations, with the names being looked up in simres.
#' One of the columns should contain the dates of the observations.Soil layer-specific
#' values are identified by the layer number appended to the variable name. E.g., to match
#' the water potential obsvered in th 4 layer, the column has to be named psimi4.
#' @param gof_fun function with arguments sim, obs
#'
#' @return named vector or list of named vectors with the Goodness-of-fit measure(s)
#' of the variables found in simres and matched to the names of observations.
#' If multiple equivalents for the names in obs are found in simres, multiple values are returned.
#'
#' @export
#'
#' @examples

calc_gof <- function(obs,
                     simres,
                     gof_fun) {

  names(obs) <- tolower(names(obs))

  # Extract ananlogues of observations from simulation results
  sim <- lapply(simres, function(x, obs) {
    names(x) <- tolower(names(x))
    obsnms <- names(obs)
    colnms <- intersect(gsub('[[:digit:]]+', '', obsnms),names(x))
    if (length(colnms) > 0) {
      x <- x[as.Date(paste(yr, mo,da), "%Y %m %d") %in% obs$dates,]

      if ("nl" %in% names(x)) {
        lays <- as.integer(gsub('[^[:digit:]]+', '', obsnms))
        x <- extract_layer_output(x,layers = lays[!is.na(lays)],
                                  value.vars = colnms )
      }
      subset(x,,intersect(obsnms, names(x)))
    }
  },obs = obs)

  # re-arrange
  sim <- as.data.frame(unlist(sim[lapply(sim,length) >0], recursive = F))
  obs <- obs[, match(sapply(strsplit(names(sim), "[.]"), tail, 1),names(obs))]

  if (!is.list(gof_fun)){
    gof_fun <- list(gof_fun)
  }
  # loop over list of gof-functions
  fit <- lapply(gof_fun, function(x) {mapply(x,sim = sim,obs=obs)})
  if(length(fit) ==1) {
    fit <- unlist(fit)
  }
  return(fit)
  }
