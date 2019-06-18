#' Transfer standproperties height, maxlai, sai, densef, age to parameter-list
#'
#' Takes a data.frame of yearly standproperties, trims/extends the columns height, maxlai,
#' sai, densef, and age for the years in out.years, and updates the provided parameter list
#'
#' @param standprop_yearly a data.frame or data.table with columns 'year', 'height', 'maxlai', 'sai', 'densef', 'age'
#' @param param.b90 a list object to update
#' @param out.years the years for which parameters should be updated
#'
#' @return the param.b90 list-object with updated items maxlai, height, height.ini,
#' sai, sai.ini, densef, densef.ini, age, age.ini.
#' @export
#'
#' @examples
#' param.b90 <- setparam_LWFB90()
#' dat <- slb1_standprop
#'
#' years <- 2002:2005
#' param.new <- standprop_yearly_to_param(dat,
#'                                        param.b90,
#'                                        years)
#'
#' identical(param.new$maxlai, dat$maxlai[dat$year %in% years])
#' identical(param.new$height, dat$height[dat$year %in% years])
#'@importFrom stats approx
standprop_yearly_to_param <- function(standprop_yearly,
                                           param.b90,
                                           out.years) {

  if (is.null(standprop_yearly)) {
    stop("standprop_yearly is missing. Required if options.b90$standprop.input = 'table'!")
  }
  stopifnot(all(c("year","height","sai", "maxlai","densef", "age") %in% names(standprop_yearly)))

  if (!any(out.years %in% standprop_yearly$year)) {
    stop("Simulation does not cover any of the years in standprop_yearly")
  }

  # transfer table to parameters, trim/extend to out.years using constant interpolation and rule = 2 in approx
  param.b90$height <- approx(x = as.Date(paste0(standprop_yearly$year,"-12-31")),
                             y = standprop_yearly$height,
                             xout = as.Date(c(paste0(out.years[1],"-01-01"),paste0(out.years,"-12-31"))),
                             method = 'constant', rule = 2)$y
  param.b90$height.ini <- param.b90$height[1]
  param.b90$height <- param.b90$height[-1]

  param.b90$sai <- approx(x = as.Date(paste0(standprop_yearly$year,"-12-31")),
                          y = standprop_yearly$sai,
                          xout = as.Date(c(paste0(out.years[1],"-01-01"),paste0(out.years,"-12-31"))),
                          method = 'constant', rule = 2)$y
  param.b90$sai.ini <- param.b90$sai[1]
  param.b90$sai <- param.b90$sai[-1]

  param.b90$densef <- approx(x = as.Date(paste0(standprop_yearly$year,"-12-31")),
                             y = standprop_yearly$densef,
                             xout = as.Date(c(paste0(out.years[1],"-01-01"),paste0(out.years,"-12-31"))),
                             method = 'constant', rule = 2)$y

  param.b90$densef.ini <- param.b90$densef[1]
  param.b90$densef <- param.b90$densef[-1]

  param.b90$maxlai <- approx(x = as.Date(paste0(standprop_yearly$year,"-01-01")),
                             y = standprop_yearly$maxlai,
                             xout = as.Date(paste0(out.years,"-01-01")),
                             method = 'constant', rule = 2)$y

  #extend or constrain age from table for out.years
  param.b90$age <- seq(standprop_yearly$age[1] - (standprop_yearly$year[1] - min(out.years)),
                       by = 1, length.out = length(out.years))
  #recalculate age.ini
  param.b90$age.ini <- param.b90$age[1] - 1

  return(param.b90)
}
