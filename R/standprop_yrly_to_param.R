#' Transfer standproperties height, maxlai, sai, densef, age to parameter-list
#'
#' Takes a data.frame of yearly standproperties, trims/extends the columns height, maxlai,
#' sai, densef, and age for the years in out_yrs, and updates the provided parameter list
#'
#' @param standprop_yearly A data.frame or data.table with columns 'year', 'height', 'maxlai', 'sai', 'densef', 'age'.
#' @param param_b90 A list object to update.
#' @param out_yrs Vector of years for which parameters should be updated.
#'
#' @return The param_b90 list-object with updated items maxlai, height, heightini,
#' sai, saiini, densef, densefini, age, ageini.
#' @export
#'
#' @examples
#' param_b90 <- setparam_LWFB90()
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
  param_b90$height <- stats::approx(x = as.Date(paste0(standprop_yearly$year,"-12-31")),
                             y = standprop_yearly$height,
                             xout = as.Date(c(paste0(out_yrs[1],"-01-01"),paste0(out_yrs,"-12-31"))),
                             method = 'constant', rule = 2)$y
  param_b90$heightini <- param_b90$height[1]
  param_b90$height <- param_b90$height[-1]

  param_b90$sai <- stats::approx(x = as.Date(paste0(standprop_yearly$year,"-12-31")),
                          y = standprop_yearly$sai,
                          xout = as.Date(c(paste0(out_yrs[1],"-01-01"),paste0(out_yrs,"-12-31"))),
                          method = 'constant', rule = 2)$y
  param_b90$saiini <- param_b90$sai[1]
  param_b90$sai <- param_b90$sai[-1]

  param_b90$densef <- stats::approx(x = as.Date(paste0(standprop_yearly$year,"-12-31")),
                             y = standprop_yearly$densef,
                             xout = as.Date(c(paste0(out_yrs[1],"-01-01"),paste0(out_yrs,"-12-31"))),
                             method = 'constant', rule = 2)$y

  param_b90$densefini <- param_b90$densef[1]
  param_b90$densef <- param_b90$densef[-1]

  param_b90$maxlai <- stats::approx(x = as.Date(paste0(standprop_yearly$year,"-01-01")),
                             y = standprop_yearly$maxlai,
                             xout = as.Date(paste0(out_yrs,"-01-01")),
                             method = 'constant', rule = 2)$y

    # extend or constrain age from table for out_yrs
  param_b90$age <- seq(standprop_yearly$age[1] - (standprop_yearly$year[1] - min(out_yrs)),
                       by = 1, length.out = length(out_yrs))

  # recalculate ageini
  param_b90$ageini <- param_b90$age[1] - 1

  return(param_b90)
}
