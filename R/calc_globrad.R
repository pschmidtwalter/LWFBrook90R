#' Calculate global solar radiation from sunshine duration hours
#'
#' Uses \code{\link[sirad]{extrat}} to determine daylength and extraterrestrial
#' radiation, from which global radiation is calculated using the
#' Angström-formula.
#'
#' @param dates Date vector
#' @param sunhours Vector of sunshine duration hours, same length as dates.
#' @param lat Latitude in decimal degrees.
#' @param a0 Angström parameter a, defaults to 0.25.
#' @param b0 Angström parameter b, defaults to 0.5.
#' @param full_output Return extraterrestrial radiation and daylength along with
#'   global radiation?
#'
#' @return A sequence of global radiation in MJ/(m² d) with the length of dates,
#'   or (if \code{full_output = TRUE}) a \code{data.frame} holding day of year,
#'   dates, sunhours, daylength, and extraterrestrial and calculated global
#'   solar radiation. A warning is generated if some sunshine duration hours are
#'   higher than the expected daylength at the specified latitude.
#'
#' @export
#' @examples
#' dates <- seq.Date(as.Date("2002-01-01"), as.Date("2003-12-31"), by = 'day')
#' calc_globrad(dates, sunhours = runif(365, 0, 7),lat = 52.8)
calc_globrad <- function(dates, sunhours, lat,
                        a0=0.25,
                        b0=0.5,
                        full_output = FALSE) {

  dat <- data.frame(dates, doy = as.integer(format(dates, "%j")), sunhours)

  exterr.DayL <- data.frame(doy = 1:366,
             daylength = dayLength(lat * pi/180, i = 1:366),
             extrat = exd(lat * pi/180, i = 1:366))

  dat <- merge(dat, exterr.DayL, by = "doy")[order(dat$dates),]
  dat$globrad <- with(dat, (a0 + b0 * sunhours / daylength) * extrat)

  if (any(dat$daylength < dat$sunhours)) {
    warning("Some sunshine duration hours seem to be higher than the expected daylength at this latitude!")
  }

  if (!full_output) {
    dat$globrad
  } else {
    dat
  }
}

#==============================================================================
#  Code from package sirad version 2.3-3
#  https://CRAN.R-project.org/package=sirad
#==============================================================================


#--  dayLength  ---------------------------------------------------------------
dayLength <- function(lat, i) {
  if (abs(degrees(lat)) < 66.5) {
    DL <-  24 * daylightTimeFactor(lat, i) / pi
  }
  if (abs(degrees(lat)) >= 66.5) {
    DL <- c()
    for (ii in 1:length(i)) {
      DLi <- length(which(exh(i = i[ii], lat = lat) > 0))
      DL <- c(DL, DLi)
    }
  }
  DL
}

degrees <- function(radians) {
  deg <- radians * 180 / pi
  deg
}

daylightTimeFactor <- function(lat, i) {
  ws <- acos(-tan(lat) * tan(solarDecl(i)))
  ws
}

solarDecl <- function(i) {
  rod <-  0.4093 * sin((2 * pi * (284 + i)) / 365)
  rod
}

exh <- function (i, lat, hr = NA, Con = 4.921) {
  rval <- c()
  for (j in i) {
    if (is.na(hr)) {
      vshr <- vector()
      for (ho in 0:23) {
        vshr <-
          c(vshr,
            Con * corrEarthSunDist(j) * cos(solarZenithAngle(lat, ho, j)))
      }
      shr <- vshr
    }
    if (is.numeric(hr)) {
      shr <- Con * corrEarthSunDist(j) * cos(solarZenithAngle(lat, hr, j))
    }
    rval <- c(rval, shr)
  }
  rval[rval < 0] <- 0
  rval
}

solarZenithAngle <- function(lat, hr, i, tr = 0.2618, hr0 = 12) {
  y <- acos(sin(lat) * sin(solarDecl(i)) +
              cos(lat) * cos(solarDecl(i)) * cos(tr * (hr - hr0)))
  y
}


#--  exd  ---------------------------------------------------------------------
exd <- function(i, lat, Con = 4.921) {
  if (abs(degrees(lat)) < 66.5) {
    Sd <-
      Con * 24 / pi * corrEarthSunDist(i) * (
        sin(lat) * sin(solarDecl(i)) * daylightTimeFactor(lat=lat, i=i) +
          cos(lat) * cos(solarDecl(i)) * sin(daylightTimeFactor(lat=lat, i=i))
      )
  }

  if (abs(degrees(lat)) >= 66.5) {
    Sd <- vector()
    for (ii in 1:length(i)) {
      ss <- exh(i = i[ii], lat = lat)

      sdi <- sum(ss[ss > 0])
      Sd <- c(Sd, sdi)
    }
  }
  Sd[Sd < 0] <- 0
  Sd  #[MJ]
}

corrEarthSunDist <- function(i) {
  d <- 1 + 0.0334 * cos(0.01721 * i - 0.0552)
  d
}


