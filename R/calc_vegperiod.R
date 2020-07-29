#' Calculate the dates of budburst and beginning of leaf fall
#'
#' Wrapper for \code{\link[vegperiod]{vegperiod}}
#'
#' @param budburst.method name of model for estimating budburst day of year. Either 'fixed'
#' or one of the values accepted by the 'start.method'-argument of the function \code{\link[vegperiod]{vegperiod}}.
#' @param leaffall.method name of model for estimating day of year when leaffall begin. Either 'fixed'
#' or one of the values accepted by the 'end.method'-argument of the function \code{\link[vegperiod]{vegperiod}}.
#' @param dates date vector passed to \code{\link[vegperiod]{vegperiod}},
#' ignored if both leaffall.method and budburst.method = 'fixed'
#' @param tavg vector of daily mean air temperature passed to \code{\link[vegperiod]{vegperiod}},
#' ignored if leaffall.method = 'fixed' and budburst.method = 'fixed'.
#' @param out.years integer vector of the years to be returned. If not specified,
#' values for the years in dates will be returned.
#' @param budburstdoy.fixed vector of values to be returned if budburst.method = 'fixed'.
#' @param leaffalldoy.fixed vector of values to be returned if leaffall.method = 'fixed'.
#' @param ... additional argument passed to \code{\link[vegperiod]{vegperiod}}.
#'
#' @return a data.frame with columns 'year', 'start', 'end'. if budburst.method = 'fixed' or leaffall.method = 'fixed',
#' 'start' and 'end' contain the values specified in budburstdoy.fixed and leaffalldoy.fixed respectively.
#' @export
#'
#' @example inst/examples/calc_vegperiod-help.R
calc_vegperiod <- function(budburst.method,
                           leaffall.method,
                           dates = NULL,
                           tavg = NULL,
                           out.years = NULL,
                           budburstdoy.fixed = 121,
                           leaffalldoy.fixed = 279,
                           ...){

  budburst.method <- match.arg(budburst.method,
                               choices = c("fixed", "Menzel","StdMeteo", "ETCCDI", "Ribes uva-crispa"))

  leaffall.method <- match.arg(leaffall.method,
                               choices = c("fixed", "vonWilpert", "LWF-BROOK90", "NuskeAlbert", "StdMeteo","ETCCDI"))

  if (is.null(dates) & is.null(tavg) & any(c(budburst.method, leaffall.method) != "fixed")) {
    stop("Please provide 'dates' and 'tavg' for using the specified methods!")
  }
  if (is.null(out.years) & is.null(dates)) {
    stop("Please provide 'out.years'!")
  }

  if (is.null(out.years)) {
    out.years <- as.integer(unique(format(dates, "%Y")))
  } else {
    if (!is.null(dates)) {
      if (!all(out.years %in% as.integer(unique(format(dates, "%Y"))))) {
        stop("date vector not covering out.years")
      }
    }
  }
  stopifnot(length(dates) == length(tavg))


  # both fixed
  if (budburst.method == "fixed" & leaffall.method == "fixed") {
    budburst_leaffall <- data.frame(year = out.years,
                                      start = budburstdoy.fixed,
                                      end = leaffalldoy.fixed)

  } else {
    if (budburst.method != "fixed" & leaffall.method != "fixed") {
      budburst_leaffall <- vegperiod::vegperiod(dates = dates,
                                                Tavg = tavg,
                                                start.method = budburst.method,
                                                end.method = leaffall.method,
                                                ...)
      budburst_leaffall <- budburst_leaffall[which(budburst_leaffall$year %in% out.years),]
    } else {
      #only start dynamic
      if (budburst.method != "fixed" & leaffall.method == "fixed")   {
        if (length(leaffalldoy.fixed) > 1 & length(leaffalldoy.fixed) != length(out.years)) {
          stop("When leaffall.method == 'fixed', either provide a single value
                 for leaffalldoy.fixed or a sequence of values, one for each in out.years.")
        }
        budburst_leaffall <- vegperiod::vegperiod(dates = dates,
                                                  Tavg = tavg,
                                                  start.method = budburst.method,
                                                  end.method = "StdMeteo",
                                                  ...)
        budburst_leaffall <- budburst_leaffall[which(budburst_leaffall$year %in% out.years),]
        budburst_leaffall$end <- leaffalldoy.fixed
      } else { #only end dynamic
        if (length(budburstdoy.fixed) > 1 & length(budburstdoy.fixed) != length(out.years)) {
          stop("When budburst.method == 'fixed', either provide a single value
                for budburstdoy.fixed or a sequence of values, one for each in out.years.")

        }
        budburst_leaffall <- vegperiod::vegperiod(dates = dates,
                                                  Tavg = tavg,
                                                  start.method = "StdMeteo",
                                                  end.method = leaffall.method,
                                                  ...)
        budburst_leaffall <- budburst_leaffall[which(budburst_leaffall$year %in% out.years),]
        budburst_leaffall$start <- budburstdoy.fixed


      }

    }
  }

  return(budburst_leaffall)
}
