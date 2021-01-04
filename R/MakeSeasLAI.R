#' Construct the seasonal course of leaf area index from parameters
#'
#' A daily sequence of leaf area index is derived from maximum and minimum
#' values, dates and shape parameters using different methods.
#'
#' @param method Name of method for generating the sequence. Must be one of
#'   "b90", "linear", "Coupmodel".
#' @param year Vector of years to be returned.
#' @param maxlai Maximum leaf are index.
#' @param winlaifrac Fraction of \code{maxlai} during winter (ignored when
#'   \code{method = 'linear'}).
#' @param budburst_doy Budburst day of year (ignored when \code{method =
#'   'linear'}).
#' @param leaffall_doy Day of year when leaf fall begins (ignored when
#'   \code{method = 'linear'}).
#' @param emerge_dur Number of days from budburst until maximum leaf area index
#'   is reached.
#' @param leaffall_dur Number of days until minimum leaf are index is reached.
#' @param shp_optdoy Day of year when optimum value is reached (required when
#'   \code{method = "Coupmodel"}).
#' @param shp_budburst Shape parameter for the growth phase (required when
#'   \code{method = "Coupmodel"}).
#' @param shp_leaffall Shape parameter growth cessation (required when
#'   \code{method = "Coupmodel"}).
#' @param lai_doy Integer vector of days of years.
#' @param lai_frac Vector of values of fractional leaf area index corresponding
#'   to lai_doy (required when \code{method = "linear"}).
#'
#' @return A vector of daily lai values covering the years specified.
#'
#' @example inst/examples/MakeSeasLAI-help.R
#' @export
MakeSeasLAI <- function(method="b90",
                        year,
                        maxlai,
                        winlaifrac = 0, #LAI in Winter
                        budburst_doy = 121, #Budburst DOY
                        leaffall_doy = 279, # DOY when leaffall is begins
                        emerge_dur = 28, # Duration until maximum LAI is reached (b90)
                        leaffall_dur = 58, # Duration until Winter LAI is reached  (b90)
                        shp_optdoy=220, #MaxLAI DOY (Coupmodel)
                        shp_budburst=0.5, #Form parameter for leaf expension period (Coupmodel)
                        shp_leaffall=10, #Form parameter for leaf Fall (Coupmodel)
                        lai_doy = c(1,121,150,280,320,365),
                        lai_frac = c(0,0,0.5,1,0.5,0)) {
  method <- match.arg(method, choices = c("b90", "linear", "Coupmodel"))

  if (method %in% c("b90", "Coupmodel")) {

    dat <- suppressWarnings(
      data.table::data.table(year = year, maxlai = maxlai, winlaifrac = winlaifrac,
                 budburst_doy = budburst_doy, leaffall_doy = leaffall_doy,
                 emerge_dur = emerge_dur, leaffall_dur = leaffall_dur,
                 shp_optdoy = shp_optdoy, shp_budburst = shp_budburst,
                 shp_leaffall = shp_leaffall)
    )

    maxdoy <- NULL; minlai <- NULL # CRAN Check Notes

    dat$maxdoy <- with(dat, ifelse( ((year %% 4 == 0) & (year %% 100 != 0)) | (year %% 400 == 0),
                          366, 365))
    dat$minlai <- with(dat, winlaifrac*maxlai)


    if (method == "b90") {
       out <- dat[, plant.b90(minval = minlai, maxval = maxlai,
                                         doy.incr = budburst_doy, incr.dur = emerge_dur,
                                         doy.decr = leaffall_doy, decr.dur = leaffall_dur,
                                         maxdoy = maxdoy),
                  by = year]$V1
     }
     if (method == "Coupmodel") {
       out <- dat[, plant.coupmodel(minval = minlai, maxval = maxlai,
                                               doy.incr = budburst_doy,
                                               doy.max = shp_optdoy,
                                               doy.min = leaffall_doy + leaffall_dur,
                                               shape.incr = shp_budburst, shape.decr = shp_leaffall,
                                               maxdoy = maxdoy),
                  by = year]$V1
     }

   } else {

     dat <- data.table::data.table(year,
                       maxdoy = ifelse( ((year %% 4 == 0) & (year %% 100 != 0)) | (year %% 400 == 0),
                                        366, 365),
                       maxlai = maxlai)
     out <- dat[, plant.linear(doys = lai_doy, values = lai_frac*maxlai, maxdoy = maxdoy),
                by = year]$V1

  }

  return(out)

}






