#' Create daily plant characteristics from parameters and options
#'
#' Creates daily sequences of 'age', 'height', 'sai', 'densef', and 'lai' from parameter and options
#' using \code{\link{approx_standprop}} and \code{\link{MakeSeasLAI}}.
#'
#' @param options.b90 a list of model control options
#' @param param.b90 a parameter list-object
#' @param out.years years for which values are returned.
#'
#' @return a data.frame containing daily sequences of 'age', 'height', 'sai', 'densef', and 'lai'.
#' @export
#'
#' @examples
#' options.b90 <- setoptions_LWFB90()
#' param.b90 <- setparam_LWFB90()
#'
#' standprop <- make_standprop(options.b90,
#'                             param.b90,
#'                             out.years = 2002:2004)
#' plot(standprop$dates, standprop$lai, type = "l")
make_standprop <- function(options.b90,
                           param.b90,
                           out.years) {

standprop_daily <- data.frame(
  dates = seq.Date(from = as.Date(paste0(min(out.years),"-01-01")),
                   to = as.Date(paste0(max(out.years),"-12-31")),
                   by = "day"),
  age = approx_standprop(x.years = out.years,
                         y = param.b90$age,
                         y.ini = param.b90$age.ini,
                         use_growthperiod = options.b90$standprop.use_growthperiod,
                         startdoy = param.b90$budburstdoy,
                         enddoy = param.b90$leaffalldoy,
                         approx.method = "linear"),
  height = approx_standprop(x.years = out.years,
                            y = param.b90$height,
                            y.ini = param.b90$height.ini,
                            use_growthperiod = options.b90$standprop.use_growthperiod,
                            startdoy = param.b90$budburstdoy,
                            enddoy = param.b90$leaffalldoy,
                            approx.method = options.b90$standprop.interp),
  sai = approx_standprop(x.years = out.years,
                         y = param.b90$sai,
                         y.ini = param.b90$sai.ini,
                         use_growthperiod = options.b90$standprop.use_growthperiod,
                         startdoy = param.b90$budburstdoy,
                         enddoy = param.b90$leaffalldoy,
                         approx.method = options.b90$standprop.interp),
  densef = approx_standprop(x.years = out.years,
                            y = param.b90$densef,
                            y.ini = param.b90$densef.ini,
                            use_growthperiod = options.b90$standprop.use_growthperiod,
                            startdoy = param.b90$budburstdoy,
                            enddoy = param.b90$leaffalldoy,
                            approx.method = options.b90$standprop.interp)
)

# daily leaf area index from parameters
standprop_daily$lai <- MakeSeasLAI(out.years,
                                     method = options.b90$lai.method,
                                     maxlai = param.b90$maxlai,
                                     winlaifrac = param.b90$winlaifrac,
                                     budburst.doy = param.b90$budburstdoy,
                                     leaffall.doy = param.b90$leaffalldoy,
                                     emerge.dur = param.b90$emergedur,
                                     leaffall.dur = param.b90$leaffalldur,
                                     shape.budburst = param.b90$shape.budburst,
                                     shape.leaffall = param.b90$shape.leaffall,
                                     shape.optdoy = param.b90$shape.optdoy,
                                     lai.doy = param.b90$lai.doy,
                                     lai.frac = param.b90$lai.frac)
return(standprop_daily)
}
