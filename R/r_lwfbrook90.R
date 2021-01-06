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
    pr = output_log,
    timer = !is.infinite(timelimit),
    n_m = as.integer(param[1]),
    n_l = as.integer(param[65])
  )

  return( list(error_code = out[[1]], daily_output = out[[2]], layer_output = out[[3]]) )
}

.onUnload <- function(libpath) {
  library.dynam.unload("LWFBrook90R", libpath)
}
