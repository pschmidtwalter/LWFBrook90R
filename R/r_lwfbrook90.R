#' Interface function to the LWF-Brook90 model
#'
#' Passes input data matrices to the Fortran model code and returns the results
#'
#' @param siteparam A [1,6] matrix with site level information: start year,
#'   start doy, latitude, initial snow, initial groundwater, precipitation
#'   interval.
#' @param climveg A matrix with 15 columns of climatic and vegetation data:
#'   year, month, day, global radiation in MJ/(m² d), tmax (deg C), tmin (deg
#'   C), vappres (kPa), wind (m/s), prec (mm), mesfl (mm), densef (-), stand
#'   height (m), lai (m²/m²), sai (m²/m²), stand age (years).
#' @param param A numeric vector of model input parameters (for the right order
#'   see \code{\link{param_to_rlwfbrook90}}).
#' @param pdur a [1,12]-matrix of precipitation durations (hours) for each
#'   month.
#' @param soil_materials A matrix of the 8 soil materials parameters. When
#'   imodel = 1 (Mualem-van Genuchten), these refer to: mat, ths, thr, alpha
#'   (1/m), npar, ksat (mm/d), tort (-), stonef (-). When imodel = 2
#'   (Clapp-Hornberger): mat, thsat, thetaf, psif (kPa), bexp, kf (mm/d), wetinf
#'   (-), stonef (-).
#' @param soil_nodes A matrix of the soil model layers with columns nl (layer
#'   number), layer midpoint (m), thickness (mm), mat, psiini (kPa), rootden
#'   (-).
#' @param precdat A matrix of precipitation interval data with 6 columns: year,
#'   month, day, interval-number (1:precint), prec, mesflp.
#' @param output_log Logical whether to print runtime output to console.
#' @param chk_input Logical whether to check for NaNs in model inputs.
#' @param timelimit Integer to set elapsed time limits for running the model.
#'
#' @return A list containing the daily and soil layer model outputs, along with
#'   an error code of the simulation (see \code{\link{run_LWFB90}}.
#'
#' @export
#' @useDynLib LWFBrook90R
#'
r_lwfbrook90 <- function(
  siteparam,
  climveg,
  param,
  pdur,
  soil_materials,
  soil_nodes,
  precdat = NULL,
  output_log = TRUE,
  chk_input = TRUE,
  timelimit  = Inf
){

  # make a matrix of precipitation input data
  if ( is.null(precdat) ){
    precdat <- matrix(-999, nrow = param[1] * siteparam[[6]], ncol = 6)
  }

  # set timeout
  setTimeLimit(elapsed = timelimit)
  on.exit(setTimeLimit(elapsed = Inf), add = TRUE)

  # Run the model
  out <- .Call(
    's_brook90_c',
    siteparam = as.matrix(siteparam, ncol = 6, nrow = 1),
    climveg = as.matrix(climveg, ncol = 15),
    param = as.vector(param),
    pdur = as.matrix( pdur, ncol = 12 ),
    soil_materials = as.matrix(soil_materials, ncol = 8),
    soil_nodes = as.matrix(soil_nodes, ncol = 6),
    precdat = as.matrix(precdat, ncol = 6),
    pr = as.integer(output_log),
    timer = as.integer(!is.infinite(timelimit)),
    chk_input = as.integer(chk_input),
    n_days = as.integer(param[1]),
    n_lays = as.integer(param[65]),
    n_pint = as.integer(siteparam[1,6])
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
                         'solnet', 'lngnet', 'aa', 'asubs'))

  # layer outputs
  out$layer_output <- data.table::rbindlist(
    lapply(seq(dim(out$layer_output)[3]),
           function(x) data.frame(out$layer_output[ , , x])),
    idcol = "nl")

  data.table::setnames(out$layer_output, names(out$layer_output)[-1],
                       c('yr','mo','da','doy','swati','theta','wetnes','psimi','infl',
                         'byfl','tran','vrfl','dsfl','ntfl'))

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

    }
  }))

}
