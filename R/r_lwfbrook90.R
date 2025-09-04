#' Interface function to the LWF-Brook90 model
#'
#' Passes input data objects (parameters and meteorology) to the Fortran model
#' code and returns the results
#'
#' @param b90f_input Brook90 input list object containing all the parameters required.
#' @param meteo A data.table of hourly meteorological data with columns: Date (dates),
#'   interval-number (hh, 1:precint), global radiation (globrad, W/m²), air temperature (tmean, deg C), rel. humidity
#'   (relhum, \%), wind speed (windspeed, m/s) (m/s), precipitation (prec, mm), and
#'   optionally longwave downward radiation (ld, W/m²).
#' @param precip  An optional data.table of precipitation interval data with 6 columns: year,
#'   month, day, interval-number (1:prec_interval), prec, mesflp.
#' @param output_log Logical whether to print runtime output to console.
#' @param chk_input Logical whether to check for NaNs in model inputs.
#' @param timelimit Integer for setting a time limit (in seconds, default: Inf) for the duration of the model execution.
#'
#' @return A list containing the daily and soil layer model outputs, along with
#'   an error code of the simulation.
#'
#' @export
#' @useDynLib LWFBrook90R
#'
r_lwfbrook90 <- function(
    b90f_input, # needs to be the complete list (check for completeness!)
    meteo,
    precip,
    output_log = TRUE,
    chk_input = TRUE,
    timelimit  = Inf
){

  # pacify check notes
  dates<-NULL; water_table_depth<-NULL

  # create parameter vector
  param_vec <- param_to_rlwfbrook90(b90f_input)

  # create site parameter vector
  siteparam_vec <- siteparam_to_rlwfbrook90(b90f_input)

  # create climveg table
  climveg <- cbind(meteo[, c("yr", "mo", "da","globrad","tmax","tmin",
                               "vappres","windspeed","prec","mesfl")],
                   b90f_input$standprop_daily[, c("densef", "height", "lai", "sai", "age")])

  # create matrix of precipitation input data
  if ( is.null(precip) ){
    precip <- matrix(-999, nrow = b90f_input$ndays * b90f_input$prec_interval, ncol = 6)
  } else {
    precip = as.matrix(precip[,c("yr", "mo", "da","ii","prec", "mesfl")],ncol = 6)
  }

  # prepare water table object
  if (data.table::is.data.table(b90f_input$water_table_depth)) {
    b90f_input$water_table_depth <- b90f_input$water_table_depth[, list(yr = data.table::year(dates),
                                                                        mo = data.table::month(dates),
                                                                        da = data.table::mday(dates),
                                                                        water_table_depth)]
  } else {
    b90f_input$water_table_depth <- b90f_input$standprop_daily[, list(yr = data.table::year(dates),
                                                                      mo = data.table::month(dates),
                                                                      da = data.table::mday(dates),
                                                                      water_table_depth = b90f_input$water_table_depth)]
  }

  # set timeout
  setTimeLimit(elapsed = timelimit)
  on.exit(setTimeLimit(elapsed = Inf), add = TRUE)

  # Run the model
  out <- .Call(
    's_brook90_c',
    siteparam = matrix(siteparam_vec, ncol = 9, nrow = 1),
    climveg = as.matrix(climveg, ncol = 15),
    param = as.vector(param_vec),
    pdur = matrix( b90f_input$pdur, ncol = 1, nrow =12),
    soil_materials = as.matrix(b90f_input$soil_materials, ncol = 8),
    soil_nodes = as.matrix(b90f_input$soil_nodes[,c("layer","midpoint", "thick", "mat", "psiini", "rootden")], ncol = 6),
    precip = precip,
    water_table_depth = as.matrix(b90f_input$water_table_depth, ncol = 4),
    pr = as.integer(output_log),
    timer = as.integer(!is.infinite(timelimit)),
    chk_input = as.integer(chk_input),
    n_days = as.integer(b90f_input$ndays),
    n_lays = as.integer(nrow(b90f_input$soil_nodes)),
    n_pint = as.integer(b90f_input$prec_interval)
  )

  names(out) <- c("error_code", "output", "layer_output" )

  ## check for simulation errors ----
  chk_errors()

  ## process and manage outputs ----

  # daily outputs
  out$output <- data.table::data.table(out$output)
  data.table::setnames(out$output, names(out$output),
                       c('yr','mo','da','doy','rfal','rint','sfal','sint','rthr','sthr','rsno',
                         'rnet','smlt','snow','swat','gwat','intr', 'ints','evap','tran','irvp',
                         'isvp','slvp','snvp','pint','ptran','pslvp','flow','seep',
                         'srfl','slfl','byfl','dsfl','gwfl','vrfln','safrac',
                         'stres','adef','awat','relawat','nits','balerr', 'slrad',
                         'solnet', 'lngnet', 'aa', 'asubs', 'snowlq', 'cc'))

  # layer outputs
  out$layer_output <- data.table::rbindlist(
    lapply(seq(dim(out$layer_output)[3]),
           function(x) data.frame(out$layer_output[ , , x])),
    idcol = "nl")

  data.table::setnames(out$layer_output, names(out$layer_output)[-1],
                       c('yr','mo','da','doy','swati','theta','wetnes','psimi','infl',
                         'byfl','tran','vrfl','dsfl','ntfl', 'relawati'))

  return(out)
}

.onUnload <- function(libpath) {
  library.dynam.unload("LWFBrook90R", libpath)
}

# check for errors -------------------------------------------------------------
chk_errors <- function(){
  eval.parent(quote({
    if (out$error_code != 0L) {
      if (out$error_code == 1L) stop("Simulation terminated abnormally: 'initial matrix psi > 0'
                                        (rerun with verbose = TRUE to see more information)")
      if (out$error_code == 2L) stop("Simulation initialization failed: 'FWETK failed to determine wetness at KF'
                                           (rerun with verbose = TRUE  to see more information)")
      if (out$error_code == 3L) stop("Simulation terminated abnormally: 'inconsistent dates in climate!'
                                        (rerun with verbose = TRUE  to see more information)")
      if (out$error_code == 4L) stop("Simulation terminated abnormally: 'inconsistent dates in precipitation input!'
                                        (rerun with verbose = TRUE  to see more information)")
      if (out$error_code == 5L) stop("Simulation terminated abnormally: 'wrong precipitation interval input!'
                                        (rerun with verbose = TRUE  to see more information)")
      if (out$error_code == 6L) stop("Simulation terminated abnormally: 'negative soil water storage!'
                                        (rerun with verbose = TRUE  to see more information)")
      if (out$error_code == 7L) stop("Simulation terminated abnormally: 'water storage exceeds water capacity!'
                                        (rerun with verbose = TRUE  to see more information)")
      if (out$error_code[[1]] == 8L) stop("Simulation terminated abnormally due to undefined elements in input!'
                                        (rerun with verbose = TRUE  to see more information)")
      if (out$error_code[[1]] == 9L) stop("Simulation terminated abnormally: bad water table definition!'
                                        (rerun with verbose = TRUE  to see more information)")

    }
  }))

}
