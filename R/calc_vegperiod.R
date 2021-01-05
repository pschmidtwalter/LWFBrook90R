#' Calculate the dates of budburst and beginning of leaf fall
#'
#' Wrapper for \code{\link[vegperiod]{vegperiod}}
#'
#' @param budburst_method name of model for estimating budburst day of year.
#'   Either 'fixed' or one of the values accepted by the 'start.method'-argument
#'   of the function \code{\link[vegperiod]{vegperiod}}.
#' @param leaffall_method name of model for estimating day of year when leaffall
#'   begin. Either 'fixed' or one of the values accepted by the
#'   'end.method'-argument of the function \code{\link[vegperiod]{vegperiod}}.
#' @param dates date vector passed to \code{\link[vegperiod]{vegperiod}},
#'   ignored if both leaffall_method and budburst_method = 'fixed'
#' @param tavg vector of daily mean air temperature (deg C) passed to
#'   \code{\link[vegperiod]{vegperiod}}, ignored if \code{leaffall_method =
#'   'fixed'} and \code{budburst_method = 'fixed'}.
#' @param out_yrs integer vector of the years to be returned. If not
#'   specified, values for the years in \code{dates} will be returned.
#' @param budburstdoy.fixed vector of values to be returned if
#'   \code{budburst_method = 'fixed'}.
#' @param leaffalldoy.fixed vector of values to be returned if
#'   \code{leaffall_method = 'fixed'}.
#' @param ... additional argument passed to \code{\link[vegperiod]{vegperiod}}.
#'
#' @return a data.frame with columns \code{year}, \code{start}, \code{end}. If
#'   \code{budburst_method = 'fixed'} or \code{leaffall_method = 'fixed'},
#'   \code{start} and \code{end} contain the values specified in
#'   \code{budburstdoy.fixed} and \code{leaffalldoy.fixed} respectively.
#' @export
#'
#' @example inst/examples/calc_vegperiod-help.R
calc_vegperiod <- function(budburst_method,
                           leaffall_method,
                           dates = NULL,
                           tavg = NULL,
                           out_yrs = NULL,
                           budburstdoy.fixed = 121,
                           leaffalldoy.fixed = 279,
                           ...){

  budburst_method <- match.arg(budburst_method,
                               choices = c("fixed", "Menzel","StdMeteo", "ETCCDI", "Ribes uva-crispa"))

  leaffall_method <- match.arg(leaffall_method,
                               choices = c("fixed", "vonWilpert", "LWF-BROOK90", "NuskeAlbert", "StdMeteo","ETCCDI"))

  if (is.null(dates) & is.null(tavg) & any(c(budburst_method, leaffall_method) != "fixed")) {
    stop("Please provide 'dates' and 'tavg' for using the specified methods!")
  }
  if (is.null(out_yrs) & is.null(dates)) {
    stop("Please provide 'out_yrs'!")
  }

  if (is.null(out_yrs)) {
    out_yrs <- as.integer(unique(format(dates, "%Y")))
  } else {
    if (!is.null(dates)) {
      if (!all(out_yrs %in% as.integer(unique(format(dates, "%Y"))))) {
        stop("date vector not covering out_yrs")
      }
    }
  }
  stopifnot(length(dates) == length(tavg))


  # both fixed
  if (budburst_method == "fixed" & leaffall_method == "fixed") {
    budburst_leaffall <- data.frame(year = out_yrs,
                                      start = budburstdoy.fixed,
                                      end = leaffalldoy.fixed)

  } else {
    if (budburst_method != "fixed" & leaffall_method != "fixed") {
      budburst_leaffall <- vegperiod::vegperiod(dates = dates,
                                                Tavg = tavg,
                                                start.method = budburst_method,
                                                end.method = leaffall_method,
                                                ...)
      budburst_leaffall <- budburst_leaffall[which(budburst_leaffall$year %in% out_yrs),]
    } else {
      #only start dynamic
      if (budburst_method != "fixed" & leaffall_method == "fixed")   {
        if (length(leaffalldoy.fixed) > 1 & length(leaffalldoy.fixed) != length(out_yrs)) {
          stop("When leaffall_method == 'fixed', either provide a single value
                 for leaffalldoy.fixed or a sequence of values, one for each in out_yrs.")
        }
        budburst_leaffall <- vegperiod::vegperiod(dates = dates,
                                                  Tavg = tavg,
                                                  start.method = budburst_method,
                                                  end.method = "StdMeteo",
                                                  ...)
        budburst_leaffall <- budburst_leaffall[which(budburst_leaffall$year %in% out_yrs),]
        budburst_leaffall$end <- leaffalldoy.fixed
      } else { #only end dynamic
        if (length(budburstdoy.fixed) > 1 & length(budburstdoy.fixed) != length(out_yrs)) {
          stop("When budburst_method == 'fixed', either provide a single value
                for budburstdoy.fixed or a sequence of values, one for each in out_yrs.")

        }
        budburst_leaffall <- vegperiod::vegperiod(dates = dates,
                                                  Tavg = tavg,
                                                  start.method = "StdMeteo",
                                                  end.method = leaffall_method,
                                                  ...)
        budburst_leaffall <- budburst_leaffall[which(budburst_leaffall$year %in% out_yrs),]
        budburst_leaffall$start <- budburstdoy.fixed


      }

    }
  }

  return(budburst_leaffall)
}
