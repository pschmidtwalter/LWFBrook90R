#' Calculate Goodness-of-fit for simulation result using observations
#'
#'
#' @param x list of data.frames with simulation results
#' @param obs data.frame of observations, with the names being looked up in x.
#' One of the columns has to refer to the dates of observations. Soil layer-specific
#' values are identified by the layer number appended to the variable name. E.g., to match
#' the water potential obsvered in th 4 layer, the column has to be named psimi4.
#' @param gof_fun function with arguments sim, obs
#'
#' @return named vector or list of named vectors with the Goodness-of-fit measure(s)
#' of the variables found in x and matched to the names of observations.
#' If multiple equivalents for the names in obs are found in x, multiple values are returned.
#'
#' @export
#'
#' @examples
#' # Set up lists containing model control options and model parameters:
#' param.b90 <- setparam_LWFB90()
#' options.b90 <- setoptions_LWFB90()
#'
#' # Set start and end Dates for the simulation
#' options.b90$startdate <- as.Date("2002-2-15")
#' options.b90$enddate <- as.Date("2003-07-5")
#'
#' # Derive soil hydraulic properties from soil physical properties
#' # using pedotransfer functions
#' soil <- cbind(slb1_soil, hydpar_wessolek_mvg(slb1_soil$texture))
#'
#' # Run LWF-Brook90
#' b90.result <- runLWFB90(project.dir = "example_run_b90",
#'                         options.b90 = options.b90,
#'                         param.b90 = param.b90,
#'                         climate = slb1_meteo,
#'                         soil = soil)
#'
#' # prepare observations
#' observations <- slb1_mpot #'daily water potential in different soil depths
#' names(observations)[2:6] <- c("psimi5", "psimi7", "psimi10", "psimi16","psimi21")
#'
#' observations <- observations[observations$dates >= options.b90$startdate
#'                              & observations$dates <= options.b90$enddate,]
#' # calculate gof- easure using simulation results and observations
#' calc_gof(obs = observations,
#'          b90.result,
#'          gof_fun = function(sim, obs) {mean(obs-sim, na.rm = TRUE)})
#' # multiple gof-measures
#' calc_gof(obs = observations, b90.result,
#'          gof_fun = list(WilmD = hydroGOF::d, bR2 = hydroGOF::br2))

calc_gof <- function(x,
                     obs,
                     gof_fun) {

  stopifnot(!is.null(obs))
  obs <- data.frame(obs)
  names(obs) <- tolower(names(obs))

  # Extract equivalents of obs from sim results
  sim <- lapply(x, function(x, obs) {
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

  sim <- as.data.frame(unlist(sim[lapply(sim,length) >0], recursive = FALSE))
  if (length(sim) == 0L) {
    warning("No matching variable names between sim and obs!")
  }

  # match observations to sim-output vars (maybe longer, due to redundancy in x)
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
