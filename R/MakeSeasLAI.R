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
#' @param lai_doy Integer vector or list of days of year (required when
#'   \code{method = "linear"}). Can be provided as a single vector which is then
#'   repeated for all years or as a named list with values for each year.
#' @param lai_frac Vector of fractional leaf area index corresponding to
#'   \code{lai_doy} (required when \code{method = "linear"}). Can be provided as
#'   a single vector which is then repeated for all years or as a named list
#'   with values for each year.
#'
#' @return A vector of daily lai values covering the years specified.
#'
#' @example inst/examples/make_seasLAI-help.R
#' @export
make_seasLAI <- function(method="b90",
                         year,
                         maxlai,
                         winlaifrac = 0, #LAI in Winter
                         budburst_doy = 121, #Budburst DOY
                         leaffall_doy = 279, # DOY when leaffall is begins
                         emerge_dur = 28, # Duration until maximum LAI is reached (b90)
                         leaffall_dur = 58, # Duration until Winter LAI is reached  (b90)
                         shp_optdoy=budburst_doy+emerge_dur, #MaxLAI DOY (Coupmodel)
                         shp_budburst=0.5, #Form parameter for leaf expension period (Coupmodel)
                         shp_leaffall=10, #Form parameter for leaf Fall (Coupmodel)
                         lai_doy = c(1,121,150,280,320,365),
                         lai_frac = c(0,0,0.5,1,0.5,0)) {
  method <- match.arg(method, choices = c("b90", "linear", "Coupmodel"))

  minlai_last_year <- NULL; doys <- NULL; values <- NULL

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

  } else {

    # --- Repeat vectors for all years if input is not a list -------------------
    if (!is.list(lai_doy)) lai_doy <- stats::setNames(rep(list(lai_doy), length(year)), as.character(year))
    if (!is.list(lai_frac)) lai_frac <- stats::setNames(rep(list(lai_frac), length(year)), as.character(year))

    # --- Check that lai_doy and lai_frac have the same length per year ----------
    for (y in names(lai_doy)) {
      if (length(lai_doy[[y]]) != length(lai_frac[[y]])) {
        stop(paste("lai_doy and lai_frac do not match in length for year:", y))
      }
    }

    # --- Create data.table from lai_doy + lai_frac --------------------------------
    lai_tbl <- data.table::rbindlist(lapply(names(lai_doy), function(y) {
      data.table(year = as.integer(y),
                 doys = lai_doy[[y]],
                 values = lai_frac[[y]])   # rename lai_frac -> values
    }))

    if (any(!year %in% lai_tbl$year)) {
      stop("Error: Some years have no lai_doy / lai_frac data")
    }

    # --- Merge with data.table of maxlai entries ---------------------------------
    dat <- data.table::data.table(year=as.integer(year), maxlai = maxlai)
    dat <- merge(dat,lai_tbl, by = "year", all.x = TRUE, allow.cartesian = TRUE)

    # --- Set max day of year ----------------------------------------------------
    dat[, maxdoy := ifelse(((year %% 4 == 0) & (year %% 100 != 0)) | (year %% 400 == 0), 366, 365)]
    setorder(dat, year, doys)

    # --- Check that doys are within maxdoy --------------------------------------
    if (any(dat$doys > max(dat$maxdoy))) {
      stop("Error: Some doys values exceed maxdoy!")
    }

    # --- Calculate absolute LAI values ------------------------------------------
    dat[, values := values * maxlai]

    # --- Interpolate LAI per year ----------------------------------------------
    out <- dat[, plant_linear(doys = doys,
                              values = values,
                              maxdoy = maxdoy[1]), by = year]$V1

  }

  return(out)

}






