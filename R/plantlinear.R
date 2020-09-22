#' Interpolate plant properties using the 'linear' method.
#'
#' Creates a daily sequence for one year from doy/value pairs.
#'
#' @param doys Vector of Dates (days of year).
#' @param values Numeric vector Values.
#' @param maxdoy Length of the year, 366 for leap years, 365 for normal years.
#'
#' @return A numeric vector of length \code{maxdoy}.
#'
#' @example inst/examples/plantlinear-help.R
#' @export
plant.linear <- function(doys, values, maxdoy) {
  stopifnot(all(doys %in% 1:maxdoy))
  stats::approx(x = doys, y = values,
         method = "linear", rule = 2,
         xout = 1:maxdoy)$y
}
