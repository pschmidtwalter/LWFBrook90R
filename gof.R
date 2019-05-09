calc_gof <- function(obs,
                     simres,
                     gof_fun) {

  stopifnot(!is.null(obs))
  names(obs) <- tolower(names(obs))

  # Extract equivalents of obs from sim results
  sim <- lapply(simres, function(x, obs) {
    names(x) <- tolower(names(x))
    obsnms <- names(obs)
    # vars without index
    colnms <- intersect(gsub('[[:digit:]]+', '', obsnms),names(x))
    if (length(colnms) > 0) {

      # constrain to simulation output
      x <- x[as.Date(paste(yr, mo,da), "%Y %m %d") %in% obs$dates,]

      # layer output
      if ("nl" %in% names(x)) {
        # extract index
        lays <- as.integer(gsub('[^[:digit:]]+', '', obsnms))
        x <- extract_layer_output(x,layers = lays[!is.na(lays)],
                                  value.vars = colnms )
      }
      subset(x, , intersect(obsnms, names(x)))
    }
  },obs = obs)

  sim <- as.data.frame(unlist(sim[lapply(sim,length) >0], recursive = F))

  # match observations to sim-output vars (maybe longer, due to redundancy in simres)
  obs <- obs[, match(sapply(strsplit(names(sim), "[.]"), tail, 1),names(obs))]

  if (!is.list(gof_fun)){
    gof_fun <- list(gof_fun)
  }
  # loop over list of gof-functions
  gof_measures <- lapply(gof_fun, function(x) {mapply(x,sim = sim, obs = obs)})

  if (length(gof_measures) == 1) {
    gof_measures <- unlist(gof_measures)
  }
  return(gof_measures)
}
