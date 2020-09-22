#' Interpolate plant properties using the 'b90' method.
#'
#' Creates a daily sequence for one year from parameters
#'
#' @param minval Minimum value.
#' @param maxval Maximum value.
#' @param doy.incr Date (day of year) when increasing to maxval begins.
#' @param incr.dur Duration (number of days) since doy.incr until maxval is reached.
#' @param doy.decr Date (day of year) when decreasing to minval begins.
#' @param decr.dur Duration (number of days) since doy.decr until minval is reached.
#' @param maxdoy Length of the year, 366 for leap years, 365 for normal years.
#'
#' @return A numeric vector of length \code{maxdoy}.
#'
#' @example inst/examples/plantb90-help.R
#' @export
plant.b90 <- function(minval, maxval,
                      doy.incr,incr.dur,
                      doy.decr, decr.dur,
                      maxdoy) {
  stopifnot(doy.incr > 0,
            doy.decr > (doy.incr+incr.dur))


  if ((doy.decr+decr.dur) > maxdoy) {
    decr.dur <- maxdoy - doy.decr
    warning("shortened decr.dur, due to doy.decr+decr.dur > maxdoy")
  }

  ind <- c(1,doy.incr, doy.incr + incr.dur,
           doy.decr, doy.decr + decr.dur,
           maxdoy)
  values <- c(minval,minval,maxval,maxval,minval,minval)
  stats::approx(x = ind, y = values,method = "linear", xout = 1:maxdoy)$y
}
