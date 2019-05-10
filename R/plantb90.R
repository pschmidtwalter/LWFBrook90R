#' Interpolate plant properties using the 'b90' method.
#'
#' Creates a daily sequence for one year from parameters
#'
#' @param minval minimum value
#' @param maxval maximum value
#' @param doy.incr date (day of year) when increasing to maxval begins
#' @param incr.dur duration (number of days) since doy.incr until maxval is rached
#' @param doy.decr date (day of year) when decreasing to minval begins
#' @param decr.dur duration (number of days) since doy.decr until minval is rached
#' @param maxdoy length of the year, 366 for leap years, 365 for normal years
#'
#' @return numeric vector of length maxdoy
#'
#' @examples
#' plot(plant.b90(minval = 0,maxval=1,
#'                doy.incr = 121,incr.dur = 28,
#'                doy.decr = 280, decr.dur = 50,
#'                maxdoy = 365))
#' @export
plant.b90 <- function(minval, maxval,
                      doy.incr,incr.dur,
                      doy.decr, decr.dur,
                      maxdoy) {
  stopifnot(doy.incr > 0,
            doy.decr > (doy.incr+incr.dur),
            (doy.decr+decr.dur) <= maxdoy)

  ind <- c(1,doy.incr, doy.incr + incr.dur,
           doy.decr, doy.decr + decr.dur,
           maxdoy)
  values <- c(minval,minval,maxval,maxval,minval,minval)
  approx(x = ind, y = values,method = "linear", xout = 1:maxdoy)$y
}
