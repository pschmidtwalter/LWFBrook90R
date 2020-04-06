#' Create a daily sequence of stand properties from parameters using interpolation
#'
#' Uses yearly values of inter-annual vegetation development values (e.g. sai, height, densef, age)
#' and interpolates them to a daily sequence.
#'
#' @param x.years A sequence of years or a single year.
#' @param y Vector of the same length as x.years. If approx.method = 'linear',
#' the values are interpreted to be valid at the end of the respective year in x.years.
#' @param y.ini Initial value used as a starting point for linear interpolation.
#' Interpreted to be valid at the 1st of January of the first year in x.years.
#' Ignored if approx.method = 'constant'.
#' @param xout.years Vector of years for which output is generated. May be longer or shorter than
#' x.years. For years outside x.years, the value of the closest data extrem is returned.
#' @param use_growthperiod Logical: Use startdoy and enddoy for linear interpolation?
#' If TRUE, yearly changes take place between startdoy and enddoy, other wise from end of year to end of year.
#' @param startdoy A single value or vector of the same length as x.years, with the day of year when growth begins.
#' @param enddoy A single value or vector of the same length as x.years, with the day of year when growth cessates.
#' @param approx.method Name of interpolation method ('constant' or 'linear').
#' @param return_xout Logical: If true, daily values of y and a date vector are returned in data.frame.
#'
#' @return A vector of interpolated daily values
#'
#' @details For \code{approx.method = 'constant'}, the value of y is returned for the whole respective year in x.years,
#' which results in a yearly changing step function. If \code{approx.method = 'linear'},
#' the values of y are interpolated between the years in x.years, and interpreted to be reached
#' at the 31st of December of the respective x.years. In this case, y.ini is required as an initial value,
#' from which the sequence is interpolated to the first value of y. The linear changes are either accomplished
#' between 31st to 31st of December of the years in x.years, or during the growing season only (use_growingperiod = TRUE).
#'
#'
#'@examples
#' options.b90 <- setoptions_LWFB90()
#' param.b90 <- setparam_LWFB90()
#'
#' years <- 2002:2004
#' options.b90$standprop.interp <- 'constant'
#' param.b90$height <- c(20.2,20.8,21.3)
#' height_c <- approx_standprop(x.years=years,
#'                              y = param.b90$height,
#'                              approx.method = options.b90$standprop.interp)
#'
#' # linear interpolation
#' options.b90$standprop.interp <- 'linear'
#' param.b90$height.ini <- 19.1
#' height_l <- approx_standprop(x.years=years,
#'                              y = param.b90$height,
#'                              y.ini = param.b90$height.ini,
#'                              approx.method = options.b90$standprop.interp)
#'
#' # use growthperiod
#' options.b90$standprop.use_growthperiod <- TRUE
#' height_l_gp <- approx_standprop(x.years = years,
#'                                 y = param.b90$height,
#'                                 y.ini = param.b90$height.ini,
#'                                 use_growthperiod = options.b90$standprop.use_growthperiod,
#'                                 startdoy = param.b90$budburstdoy,
#'                                 enddoy = param.b90$leaffalldoy,
#'                                 approx.method = options.b90$standprop.interp)
#'
#' dates <- seq.Date(from = as.Date(paste0(min(years),"-01-01")),
#'                   to = as.Date(paste0(max(years),"-12-31")),
#'                   by = "day")
#' plot(dates, height_c,
#'      type = "l", lwd = 2, col = "black",
#'      ylim = c(19,22), ylab = "height [m]", xlab = "", xpd = TRUE)
#' lines(dates, height_l,
#'       col = "blue", lwd = 2)
#' lines(dates, height_l_gp,
#'       col = "green", lwd = 2)
#' legend("topleft", legend = c("'constant'",
#'                              "'linear'", "'linear', 'use_growthperiod'"),
#'        col  = c("black", "blue", "green"),  lwd = 2, pch = NULL,
#'        bty = "n")
#'@export
approx_standprop <- function(x.years,
                             y,
                             y.ini = NULL,
                             xout.years = x.years,
                             use_growthperiod = FALSE,
                             startdoy = 121,
                             enddoy = 279,
                             approx.method = "constant",
                             return_xout = FALSE) {

  # Repeat y for x.years, if it is a single value
  if (length(y) == 1) {
    y <- rep(y, times = length(x.years))
  }

  if (length(y) != length(x.years)) {
    stop("Please either provide a single value as y, or provide a vector of y-values of the same length as x.years!")
  }


  if (approx.method == "linear") {

    if (is.null(y.ini)) {
      stop("Please provide an initial value for y (y.ini) as a starting point for linear interpolation!")
    }

    # construct suspension points for interpolation
    if (use_growthperiod) {

      # if (length(startdoy) != length(enddoy)) {
      #   stop("startdoy and enddoy must the same length!" )
      # }

      if (length(startdoy) > 1 & length(startdoy) != length(x.years)) {
        stop("Please provide single values for startdoy, or a vector of the same length as x.years!" )
      }
      if (length(enddoy) > 1 & length(enddoy) != length(x.years)) {
        stop("Please provide single values for enddoy, or a vector of the same length as x.years!" )
      }

      x.dates <- c(as.Date(paste0(x.years[1],"-01-01")),
                   as.Date(paste(rep(x.years,each = 2),
                                 as.vector(rbind(startdoy,enddoy)),
                                 sep = "-"),
                           "%Y-%j"),
                   as.Date(paste0(x.years[length(x.years)],"-12-31")))

      y <- rep(c(y.ini,y), each = 2)

    } else { #no growthperiod
      x.dates <- as.Date(c(paste0(x.years[1],"-01-01"), paste0(x.years,"-12-31")), "%Y-%m-%d")
      y <- c(y.ini, y)
    }

  } else { # constant interpolation

    x.dates <- as.Date(paste0(x.years,"-12-31"), "%Y-%m-%d")
    # y remains unchanged.
  }

  # contruct output x-sequence
  xout.dates <- seq.Date(from = as.Date(paste0(min(xout.years),"-01-01")),
                         to = as.Date(paste0(max(xout.years),"-12-31")),
                         by = "day")


  #interpolate
  if (length(y) == 1) { # all constant
    yout <- rep(y, times = length(xout.dates))

  } else { # interpolation
    yout <- stats::approx(x.dates, y, xout = xout.dates, rule = 2, f = 1, method = approx.method)$y
  }

  if (!return_xout) {
    return(yout)
  } else {
    return(data.frame(xout = xout.dates, yout))
  }

}
