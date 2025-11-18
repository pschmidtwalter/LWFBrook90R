#' Interpolate plant properties using the 'b90' method
#'
#' Creates a daily sequence for one year from parameters.
#'
#' @param minval_before_incr Minimum value used for the time period before
#'   \code{doy.incr}.
#' @param minval_after_decr Minimum value used for time period after
#'   \code{doy.decr}.
#' @param maxval Maximum value.
#' @param doy.incr Day of year when increasing from \code{minval_before_incr} to
#'   \code{maxval} starts.
#' @param incr.dur Duration (number of days) since  \code{doy.incr} until
#'   \code{maxval} is reached.
#' @param doy.decr Day of year when decreasing to \code{minval_after_decr}
#'   begins.
#' @param decr.dur Duration (number of days) since \code{doy.incr} until
#'   \code{minval_after_decr} is reached.
#' @param maxdoy Length of the year (366 for leap years).
#'
#' @return A numeric vector of length \code{maxdoy}.
#'
#' @examples
#' plot(plant_b90(minval_before_incr = 0,minval_after_decr = 0.2,
#' maxval=1,
#' doy.incr = 121,incr.dur = 28,
#' doy.decr = 280, decr.dur = 50,
#' maxdoy = 365))
#' @export
plant_b90 <- function(minval_before_incr,
                      minval_after_decr = minval_before_incr,
                      maxval,
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
  values <- c(minval_before_incr,minval_before_incr,maxval,maxval,minval_after_decr,minval_after_decr)
  stats::approx(x = ind, y = values,method = "linear", xout = 1:maxdoy)$y
}
