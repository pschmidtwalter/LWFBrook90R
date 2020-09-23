#' Interpolate plant properties using the 'linear' method.
#'
#' Creates a daily sequence for one year from doy/value pairs.
#'
#' @param doys Integer vector of dates (days of year).
#' @param values Numeric vector of values.
#' @param maxdoy Integer length of the year, 366 for leap years, 365 for normal years.
#'
#' @return A numeric vector of length \code{maxdoy}.
#'
#' @examples
#' doys <- c(110,200,250,280)
#' values <-  c(0,0.8,1,0)
#' maxdoy <- 365
#' plot(plant.linear(doys = doys, values = values, maxdoy = 365))
#' @export
plant.linear <- function(doys, values, maxdoy) {
  stopifnot(all(doys %in% 1:maxdoy))
  stats::approx(x = doys, y = values,
         method = "linear", rule = 2,
         xout = 1:maxdoy)$y
}
