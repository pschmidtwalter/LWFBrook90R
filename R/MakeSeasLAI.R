#' Construct the seasonal course of leaf area index from parameters
#'
#' A daily sequence of leaf area index is derived from maximum and minimum
#' values, dates and shape parameters using different methods.
#'
#' @param method Name of method for generating the sequence. Must be one of
#'   "b90", "linear", "Coupmodel".
#' @param years Vector of years to be returned.
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
#' @param lai_doy_table Data.frame with day of year ('doy') and LAI fraction
#'   values ('lai_frac'), required when \code{method = "linear"}). Can be
#'   provided as a single data.frame or a list of data.frames with one entry for
#'   each year of the simulation.
#'
#' @return A vector of daily lai values covering the years specified.
#'
#' @example inst/examples/make_seasLAI-help.R
#' @export
make_seasLAI <- function(method="b90",
                         years,
                         maxlai,
                         winlaifrac = 0, #LAI in Winter
                         budburst_doy = 121, #Budburst DOY
                         leaffall_doy = 279, # DOY when leaffall is begins
                         emerge_dur = 28, # Duration until maximum LAI is reached (b90)
                         leaffall_dur = 58, # Duration until Winter LAI is reached  (b90)
                         shp_optdoy=budburst_doy+emerge_dur, #MaxLAI DOY (Coupmodel)
                         shp_budburst=0.5, #Form parameter for leaf expension period (Coupmodel)
                         shp_leaffall=10, #Form parameter for leaf Fall (Coupmodel)
                         lai_doy_table = data.frame(lai_doy = c(1,121,150,280,320,365),
                                                  lai_frac = c(0,0,0.5,1,0.5,0))
                         ) {
  method <- match.arg(method, choices = c("b90", "linear", "Coupmodel"))

  minlai_last_year <- NULL; doys <- NULL; values <- NULL; minlai <- NULL# CRAN Check Notes

  maxdoy <- ifelse( ((years %% 4 == 0) & (years %% 100 != 0)) | (years %% 400 == 0),
                    366, 365)

  if (method %in% c("b90", "Coupmodel")) {

    dat <- suppressWarnings(
      data.table::data.table(year = years, maxlai = maxlai, winlaifrac = winlaifrac,
                             budburst_doy = budburst_doy, leaffall_doy = leaffall_doy,
                             emerge_dur = emerge_dur, leaffall_dur = leaffall_dur,
                             shp_optdoy = shp_optdoy, shp_budburst = shp_budburst,
                             shp_leaffall = shp_leaffall,
                             maxdoy = maxdoy)
    )

    dat$minlai <- with(dat, winlaifrac*maxlai)
    # take minlai from last year for start
    dat[, minlai_last_year := shift(minlai, n = 1, type = "lag", fill = NA)]
    # If previous year doesn't exist, take current year's minlai
    dat[is.na(minlai_last_year), minlai_last_year := minlai]


    if (method == "b90") {
      out <- dat[, plant_b90(minval_before_incr = minlai_last_year,
                             minval_after_decr = minlai, maxval = maxlai,
                             doy.incr = budburst_doy, incr.dur = emerge_dur,
                             doy.decr = leaffall_doy, decr.dur = leaffall_dur,
                             maxdoy = maxdoy),
                 by = year]$V1
    }
    if (method == "Coupmodel") {
      out <- dat[, plant_coupmodel(minval_before_incr = minlai_last_year,
                                   minval_after_decr = minlai, maxval = maxlai,
                                   doy.incr = budburst_doy,
                                   doy.max = shp_optdoy,
                                   doy.min = leaffall_doy + leaffall_dur,
                                   shape.incr = shp_budburst, shape.decr = shp_leaffall,
                                   maxdoy = maxdoy),
                 by = year]$V1
    }

  } else if (method == "linear"){

    # ---  Plant linear -------------------
    # Single data.frame input: replicate each year and interpolate
    if (is.data.frame(lai_doy_table)) {
      # single data.frame: replicate for each year
      lai <- lapply(1:length(years), function(y) {
        data.frame(year =rep(years[y], maxdoy[y]),
                   doy = 1:maxdoy[y],
                   lai = plant_linear(doys = lai_doy_table$lai_doy,
                                      values = lai_doy_table$lai_frac*maxlai[y],
                                      maxdoy = maxdoy[y]))
      })
    } else { # list-input
      lai <- lapply(1:length(years), function(y) {data.frame(year =rep(years[y], maxdoy[y]),
                                                            doy = 1:maxdoy[y],
                                                            lai = plant_linear(doys = lai_doy_table[[y]]$lai_doy,
                                                                               values = lai_doy_table[[y]]$lai_frac*maxlai[y],
                                                                               maxdoy = maxdoy[y]))
      })
    }

    out <- rbindlist(lai)$lai
  } else {
    stop("Unknown lai_method. Possible methods: 'b90', 'linear', 'Coupmodel'")
  }

  return(out)

}






