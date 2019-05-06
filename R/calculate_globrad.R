#' Calculate global solar radiation from sunshine duration hours
#'
#' Uses day of year (doy) and latitude to determine extraterrestrial solar radiation and daylength,
#' which is then used to calculate global radiation using angström formula
#'
#' @param doy day of year
#' @param SunHour sunshine duration hours, same length as doy
#' @param lat latitude in decimal degrees
#' @param a0 angström parameter a, defaults to 0.25
#' @param b0 angström parameter b, defaults to 0.5
#'
#' @return a sequence of global radiation in \eqn{MJ m^-2 d^-1} with the length of doy
#' @export
CalcGlobRad <- function(doy, SunHour, lat,
                        a0=0.25,
                        b0=0.5
) {

  if (!requireNamespace("sirad", quietly = TRUE)) {
    stop("Package \"sirad\" needed for calculating global radiation from sunshine duration. Please install it.")
  }

  latrad <- pi/180 * lat

  exterr.DayL <- data.frame(DayLength = sirad:::dayLength(latrad, i = doy),
                            ExtraTerrestrialSolarRadiationDaily = sirad:::exd(latrad, i = doy))


  return( (a0 + b0 * SunHour / exterr.DayL$DayLength) * exterr.DayL$ExtraTerrestrialSolarRadiationDaily )
}

