#' Calculate global solar radiation from sunshine duration hours
#'
#' Uses day of year (doy) and latitude to determine extraterrestrial solar radiation and daylength,
#' which is then used to calculate global radiation using angström formula
#'
#' @param doy day of year
#' @param sunhours sunshine duration hours, same length as doy
#' @param lat latitude in decimal degrees
#' @param a0 angström parameter a, defaults to 0.25
#' @param b0 angström parameter b, defaults to 0.5
#'
#' @return a sequence of global radiation in \eqn{MJ m^{-2} d^{-1}} with the length of doy
#'
#' @export
#' @examples
#' calc_globrad(doy = 1:365, sunhours = runif(365, 0, 8),lat = 52.8)
calc_globrad <- function(doy, sunhours, lat,
                        a0=0.25,
                        b0=0.5
) {

  if (!requireNamespace("sirad", quietly = TRUE)) {
    stop("Package \"sirad\" needed for calculating global radiation from sunshine duration. Please install it.")
  }

  latrad <- pi/180 * lat

  exterr.DayL <- sirad::extrat(doy,latrad)[c(1,3)]

  return( (a0 + b0 * sunhours / exterr.DayL$DayLength) * exterr.DayL$ExtraTerrestrialSolarRadiationDaily )
}

