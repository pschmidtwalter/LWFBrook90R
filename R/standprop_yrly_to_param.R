#' Transfer standproperties height, maxlai, sai, densef, age to parameter list object
#'
#' Takes a data.frame of yearly stand properties, trims/extends the columns height, maxlai,
#' sai, densef, and age for the years in \code{out_yrs}, and updates the provided parameter list.
#'
#' @param standprop_yearly A data.frame or data.table with columns 'year', 'height', 'maxlai', 'sai', 'densef', 'age'.
#' @param param_b90 A list object to update.
#' @param out_yrs Vector of years for which parameters should be updated.
#'
#' @return The param_b90 list-object with updated items maxlai, height, height_ini,
#' sai, sai_ini, densef, densef_ini, age, age_ini.
#'
#' @export
#'
#' @examples
#' param_b90 <- set_paramLWFB90()
#' dat <- slb1_standprop
#'
#' years <- 2002:2005
#' param.new <- standprop_yearly_to_param(dat,
#'                                        param_b90,
#'                                        years)
#'
#' identical(param.new$maxlai, dat$maxlai[dat$year %in% years])
#' identical(param.new$height, dat$height[dat$year %in% years])

standprop_yearly_to_param <- function(standprop_yearly,
                                      param_b90,
                                      out_yrs) {

  if (is.null(standprop_yearly)) {
    stop("standprop_yearly is missing. Required if options_b90$standprop_input = 'table'!")
  }
  stopifnot(all(c("year","height","sai", "maxlai","densef", "age") %in% names(standprop_yearly)))

  if (!any(out_yrs %in% standprop_yearly$year)) {
    stop("Simulation does not cover any of the years in standprop_yearly")
  }

  # transfer table to parameters, trim/extend to out_yrs using constant interpolation and rule = 2 in approx

  param_b90$height <- stats::approx(x = standprop_yearly$year,
                                    y = standprop_yearly$height,
                                    xout = c(out_yrs[1]-1, out_yrs),
                                    method = 'constant', rule = 2)$y
  param_b90$height_ini <- param_b90$height[1]
  param_b90$height <- param_b90$height[-1]

  param_b90$sai <- stats::approx(x = standprop_yearly$year,
                                 y = standprop_yearly$sai,
                                 xout = c(out_yrs[1]-1, out_yrs),
                                 method = 'constant', rule = 2)$y
  param_b90$sai_ini <- param_b90$sai[1]
  param_b90$sai <- param_b90$sai[-1]

  param_b90$densef <- stats::approx(x = standprop_yearly$year,
                                    y = standprop_yearly$densef,
                                    xout = c(out_yrs[1]-1, out_yrs),
                                    method = 'constant', rule = 2)$y
  param_b90$densef_ini <- param_b90$densef[1]
  param_b90$densef <- param_b90$densef[-1]

  param_b90$maxlai <- stats::approx(x = standprop_yearly$year,
                                    y = standprop_yearly$maxlai,
                                    xout = out_yrs,
                                    method = 'constant', rule = 2)$y

  # extend or constrain age from table for out_yrs
  param_b90$age <- seq(standprop_yearly$age[1] - (standprop_yearly$year[1] - min(out_yrs)),
                       by = 1, length.out = length(out_yrs))

  # recalculate age_ini
  param_b90$age_ini <- param_b90$age[1] - 1

  return(param_b90)
}
