#' Interpolate plant properties using the 'linear' method.
#'
#' Creates a daily sequence for one year from doy/value pairs.
#'
#' @param doys dates (days of year)
#' @param values values
#' @param maxdoy length of the year, 366 for leap years, 365 for normal years
#'
#' @return numeric vector of length maxdoy
#'
#' @examples
#' plot(plant.linear(c(110,200,250,280), c(0,0.8,1,0)), 365)
#' @export
plant.linear <- function(doys, values, maxdoy) {
  inddays <- unique(c(1,doys,maxdoy))
  approx(x = inddays, y = values, method = "linear", rule = 2, xout = 1:maxdoy)$y
}
