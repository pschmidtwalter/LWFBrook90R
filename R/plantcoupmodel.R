#' Interpolate plant properties using the 'Coupmodel' method.
#'
#' Creates a daily sequence for one year from parameters
#'
#' @param minval Minimum value.
#' @param maxval Maximum value.
#' @param doy.incr Day of year when increasing from \code{minval} to
#'   \code{maxval} begins.
#' @param doy.max  Day of year when \code{maxval} is reached.
#' @param doy.min Day of year when \code{minval} is reached again.
#' @param shape.incr Shape parameter of the increasing phase.
#' @param shape.decr Shape parameter of the decreasing phase.
#' @param maxdoy Length of the year, 366 for leap years, 365 for normal years.
#'
#' @return A numeric vector of length \code{maxdoy}.
#'
#' @references
#' Jansson, P.-E. & Karlberg, L. (2004): "Coupled heat and mass transfer model for
#' soil-plant-atmosphere systems."
#' \emph{Royal Institute of Technolgy, Dept of Civil and Environmental Engineering Stockholm}
#'
#' @examples
#' plot(plant_coupmodel(0,5, 121, 200, 280, 0.3, 3, 365))
#'
#' @export
plant_coupmodel <- function(minval,
                            maxval,
                            doy.incr,
                            doy.max,
                            doy.min,
                            shape.incr,
                            shape.decr,
                            maxdoy) {

  inddays <- as.integer(c(1,doy.incr,doy.max,doy.min,maxdoy))
  values <- c(minval, minval,maxval,minval,minval)

  forms <- c(1,shape.incr,shape.decr,1)

  ind <- c(rep(1L, as.integer(doy.incr) - 1),
           rep(2L, as.integer(doy.max) - as.integer(doy.incr)),
           rep(3L, as.integer(doy.min) - as.integer(doy.max)),
           rep(4L, maxdoy - as.integer(doy.min) + 1))

  doy <- 1:maxdoy

  alpha <-  sin(
    (doy - inddays[ind]) /  (inddays[ind+1] - inddays[ind]) * pi / 2) ^ (forms[ind])

  (1 - alpha)*values[ind] + alpha * values[ind+1]

}
