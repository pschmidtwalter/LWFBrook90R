#' Create daily plant characteristics from parameters and options
#'
#' Creates daily sequences of 'age', 'height', 'sai', 'densef', and 'lai' from
#' parameters and options using \code{\link{approx_standprop}} and
#' \code{\link{make_seasLAI}}.
#'
#' @param options_b90 A list of model control options.
#' @param param_b90 A parameter list-object.
#' @param out_yrs Years for which values are returned.
#'
#' @return A data.frame containing daily sequences of 'age', 'height', 'sai',
#'   'densef', and 'lai'.
#' @export
#'
#' @example inst/examples/make_standprop-help.R

make_standprop <- function(options_b90,
                           param_b90,
                           out_yrs) {

  standprop_daily <- data.frame(
    dates = seq.Date(from = as.Date(paste0(min(out_yrs),"-01-01")),
                     to = as.Date(paste0(max(out_yrs),"-12-31")),
                     by = "day"),
    age = approx_standprop(x_yrs = out_yrs,
                           y = param_b90$age,
                           y_ini = param_b90$age_ini,
                           use_growthperiod = options_b90$use_growthperiod,
                           startdoy = param_b90$budburstdoy,
                           enddoy = param_b90$leaffalldoy,
                           approx.method = "linear"),
    height = approx_standprop(x_yrs = out_yrs,
                              y = param_b90$height,
                              y_ini = param_b90$height_ini,
                              use_growthperiod = options_b90$use_growthperiod,
                              startdoy = param_b90$budburstdoy,
                              enddoy = param_b90$leaffalldoy,
                              approx.method = options_b90$standprop_interp),
    sai = approx_standprop(x_yrs = out_yrs,
                           y = param_b90$sai,
                           y_ini = param_b90$sai_ini,
                           use_growthperiod = options_b90$use_growthperiod,
                           startdoy = param_b90$budburstdoy,
                           enddoy = param_b90$leaffalldoy,
                           approx.method = options_b90$standprop_interp),
    densef = approx_standprop(x_yrs = out_yrs,
                              y = param_b90$densef,
                              y_ini = param_b90$densef_ini,
                              use_growthperiod = options_b90$use_growthperiod,
                              startdoy = param_b90$budburstdoy,
                              enddoy = param_b90$leaffalldoy,
                              approx.method = options_b90$standprop_interp)
  )

  # daily leaf area index from parameters
  standprop_daily$lai <- make_seasLAI(out_yrs,
                                      method = options_b90$lai_method,
                                      maxlai = param_b90$maxlai,
                                      winlaifrac = param_b90$winlaifrac,
                                      budburst_doy = param_b90$budburstdoy,
                                      leaffall_doy = param_b90$leaffalldoy,
                                      emerge_dur = param_b90$emergedur,
                                      leaffall_dur = param_b90$leaffalldur,
                                      shp_budburst = param_b90$shp_budburst,
                                      shp_leaffall = param_b90$shp_leaffall,
                                      shp_optdoy = param_b90$shp_optdoy,
                                      lai_doy_tbl = param_b90$lai_doy_tble)
  return(standprop_daily)
}
