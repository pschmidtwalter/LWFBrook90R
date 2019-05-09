#' Construct the seasonal course of leaf area index from parameters
#'
#' A daily sequence of leaf area index is derived from maximum and minimum values,
#' dates and shape parameters using different methods.
#'
#' @param method name of method for generating the sequence. Must be one of "b90", "linear", "Coupmodel"
#' @param year years to be returned
#' @param maxlai maximum value during summer
#' @param winlaifrac fraction of maxlai during winter (ignored when method = 'linear')
#' @param budburst.doy budburst day of year (ignored when method = 'linear')
#' @param leaffall.doy day of year when leaf fall begins (ignored when method = 'linear')
#' @param emerge.dur number of days from budburst until maximum leaf area index is reached
#' @param leaffall.dur number of days until minimum leaf are index is reached
#' @param shape.optdoy day of year when optimum value is reached (required when method = "Coupmodel")
#' @param shape.budburst shape parameter for the growth phase (required when method = "Coupmodel")
#' @param shape.leaffall shape parameter growth cessation (required when method = "Coupmodel")
#' @param lai.doy integer vector of days of years
#' @param lai.frac vector of values of fractional leaf area index corresponding
#' to lai.doy (required when method = "linear")
#'
#' @return  a vector of daily lai values covering the years specified
#'
#' @examples
#'
#' # lai courses
#' lai_b90 <- MakeSeasLAI(method = "b90",
#'                        year = 2001,
#'                        maxlai = 5,
#'                        winlaifrac = 0,
#'                        budburst.doy = 121,
#'                        leaffall.doy = 280,
#'                        emerge.dur = 15,
#'                        leaffall.dur = 30)
#'
#' lai.doy <- c(1,110,117,135,175,220,250,290,365)
#' lai.frac <- c(0.1,0.1,0.5,0.7,1.2,1.2,1.0,0.1,0.1)
#' lai_linear <- MakeSeasLAI(method = "linear",
#'                           year = 2001,
#'                           maxlai = 5,
#'                           lai.doy = lai.doy,
#'                           lai.frac = lai.frac)
#'
#' lai_coupmodel <- MakeSeasLAI(method = "Coupmodel",
#'                              year = 2001,
#'                              maxlai = 5,
#'                              winlaifrac = 0.1,
#'                              budburst.doy = 110,
#'                              leaffall.doy = 280,
#'                              shape.optdoy = 180,
#'                              shape.budburst = 0.5,
#'                              shape.leaffall = 5)
#'
#' plot(lai_b90, type = "n", xlab = "doy", ylab = "lai [m²/m²]", ylim = c(0,6))
#' lines(lai_b90, col ="green",lwd = 2,)
#' lines(lai_linear, col ="red",lwd = 2)
#' lines(lai_coupmodel, col ="blue",lwd = 2)
#'
#' # incorparating between-year variability
#' years <- 2001:2003
#' lai <- MakeSeasLAI(method = "Coupmodel",
#'                    year = years,
#'                    maxlai = c(4,6,5),
#'                    budburst.doy = c(100,135,121),
#'                    leaffall.doy = 280,
#'                    shape.budburst = c(3,1,0.3),
#'                    shape.leaffall = 3,
#'                    shape.optdoy =c(210,180,240) )
#'
#' dates <- seq.Date(as.Date("2001-01-01"), as.Date("2003-12-31"), by = "day")
#' plot(dates,lai, col = "green", ylab = "lai [m²/m²]",
#'      type ="l", xlab = "", lwd = 2)
#'
#' @export
MakeSeasLAI <- function(method="b90",
                        year,
                        maxlai,
                        winlaifrac = 0, #LAI in Winter
                        budburst.doy = 121, #Budburst DOY
                        leaffall.doy = 279, # DOY when leaffall is begins
                        emerge.dur = 28, # Duration until maximum LAI is reached (b90)
                        leaffall.dur = 58, # Duration until Winter LAI is reached  (b90)
                        shape.optdoy=220, #MaxLAI DOY (Coupmodel)
                        shape.budburst=0.5, #Form parameter for leaf expension period (Coupmodel)
                        shape.leaffall=10, #Form parameter for leaf Fall (Coupmodel)
                        lai.doy = c(1,121,150,280,320,365),
                        lai.frac = c(0,0,0.5,1,0.5,0)) {
  method <- match.arg(method, choices = c("b90", "linear", "Coupmodel"))

  if (method %in% c("b90", "Coupmodel")) {

    dat <- suppressWarnings(
      data.table::data.table(year = year, maxlai = maxlai, winlaifrac = winlaifrac,
                 budburst.doy = budburst.doy, leaffall.doy = leaffall.doy,
                 emerge.dur = emerge.dur, leaffall.dur = leaffall.dur,
                 shape.optdoy = shape.optdoy, shape.budburst = shape.budburst,
                 shape.leaffall = shape.leaffall)
    )

    dat[,maxdoy := ifelse( ((year %% 4 == 0) & (year %% 100 != 0)) | (year %% 400 == 0),
                           366, 365)]
    dat[, minlai := winlaifrac*maxlai]


     if (method == "b90") {
       out <- dat[, plant.b90(minval = minlai, maxval = maxlai,
                                         doy.incr = budburst.doy, incr.dur = emerge.dur,
                                         doy.decr = leaffall.doy, decr.dur = leaffall.dur,
                                         maxdoy = maxdoy),
                  by = year]$V1
     }
     if (method == "Coupmodel") {
       out <- dat[, plant.coupmodel(minval = minlai, maxval = maxlai,
                                               doy.incr = budburst.doy,
                                               doy.max = shape.optdoy,
                                               doy.min = leaffall.doy + leaffall.dur,
                                               shape.incr = shape.budburst, shape.decr = shape.leaffall,
                                               maxdoy = maxdoy),
                  by = year]$V1
     }

   } else {

     dat <- data.table::data.table(year,
                       maxdoy = ifelse( ((year %% 4 == 0) & (year %% 100 != 0)) | (year %% 400 == 0),
                                        366, 365),
                       maxlai = maxlai)
     out <- dat[, plant.linear(doys = lai.doy, values = lai.frac*maxlai, maxdoy = maxdoy),
                by = year]$V1

  }

  return(out)

}






