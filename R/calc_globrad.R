#' Calculate global solar radiation from sunshine duration hours
#'
#' Uses \code{\link[sirad]{extrat}} to determine daylength and extraterrestrial radiation,
#' from which global radiation is calculated using the angström formula.
#'
#' @param dates Date vector
#' @param sunhours vector of sunshine duration hours, same length as dates
#' @param lat latitude in decimal degrees
#' @param a0 angström parameter a, defaults to 0.25
#' @param b0 angström parameter b, defaults to 0.5
#' @param full.output return extraterrestrial radiation and daylength along with global radiation?
#'
#' @return a sequence of global radiation in \eqn{MJ m^{-2} d^{-1}} with the length of dates,
#' or (if full.output = TRUE) a data.frame holding day of year, dates, sunhours, daylength, and
#' extraterrestrial and calculated global solar radiation.
#' A warning is generated if some sunshine duration hours are higher than the
#' expected daylength at the specified latitude.
#'
#' @export
#' @examples
#' dates <- seq.Date(as.Date("2002-01-01"), as.Date("2003-12-31"), by = 'day')
#' calc_globrad(dates, sunhours = runif(365, 0, 8),lat = 52.8)
calc_globrad <- function(dates, sunhours, lat,
                        a0=0.25,
                        b0=0.5,
                        full.output = FALSE) {

  dat <- data.frame(dates, doy = as.integer(format(dates, "%j")), sunhours)


  latrad <- sirad::radians(lat)

  exterr.DayL <- data.frame(doy = 1:366, sirad::extrat(1:366,latrad)[c(1,3)])

  dat <- merge(dat, exterr.DayL, by = "doy")[order(dat$dates),]
  dat$globrad <- with(dat, (a0 + b0 * sunhours / DayLength) * ExtraTerrestrialSolarRadiationDaily)

  if (any(dat$DayLength < dat$sunhours)) {
    warning("Some sunshine duration hours seem to be higher than the expected daylength at this latitude!")
  }

  if (!full.output) {
    dat$globrad
  } else {
    dat
  }
}

