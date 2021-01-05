#' Create a list of model control options
#'
#' @param ... Named values to be included in return value.
#'
#' @return A list of model control options for use as
#'   \code{options_b90}-argument in \code{\link{runLWFB90}}.
#'
#' @details
#' \describe{
#'   \item{startdate}{start date of the simulation.}
#'   \item{enddate}{end date of the simulation.}
#'   \item{fornetrad}{use global solar radiation (\code{='globrad'}) or sunshine
#'   duration hours (\code{='sunhours'}) for net radiation calculation?}
#'   \item{prec_interval}{number of precipitation intervals per day (default is
#'   1). If \code{prec_interval > 1}, the \code{precip}-argument has to be
#'   provided to \code{\link{runLWFB90}}}
#'   \item{prec_corr}{correct precipitation data for wind and evaporation losses
#'   using \code{\link{prec_corr}}?} \item{budburst_method}{name of method for
#'   budburst calculation. If \code{'constant'} or \code{'fixed'}, budburst day
#'   of year from parameters is used. All other methods calculate budburst day
#'   of year dynamically from airtemperatures, and the method name is passed to
#'   the \code{start.method}-argument of  \code{\link[vegperiod]{vegperiod}}.}
#'   \item{leaffall_method}{name of method for leaffall calculation. If
#'   \code{'constant'} or \code{'fixed'}, beginning of leaffall (day of year)
#'   from parameters is used. All other methods calculate budburst day of year
#'   dynamically from temperatures, and the method name is passed to the
#'   \code{end.method}-argument of \code{\link[vegperiod]{vegperiod}}.}
#'   \item{standprop_input}{name of input for longterm (interannual) plant
#'   development. \code{standprop_input = 'parameters'}: yearly values of stand
#'   properties height, sai, densef, lai are taken from individual parameters,
#'   \code{standprop_input = 'table'}:  values from \code{standprop_table}
#'   provided in parameters are used.} \item{standprop_interp}{interpolation
#'   method for aboveground stand properties. \code{'linear'} or
#'   \code{'constant'}, see \code{approx.method}-argument of
#'   \code{\link{approx_standprop}}.} \item{use_growthperiod}{Should
#'   yearly changes of stand properties (growth) only take place during the
#'   growth period? If \code{TRUE}, linear interpolation of height, sai, densef
#'   and age are made from budburst until leaffall. During winter values are
#'   constant. Beginning and end of the growth period are taken from parameters
#'   \code{budburstdoy} and \code{leaffalldoy}. See \code{use_growthperiod}-argument of
#'   \code{\link{approx_standprop}}.} \item{lai_method}{name of method for
#'   constructing seasonal course leaf area index development from parameters.
#'   Passed to \code{method}-argument of \code{\link{makeSeasLAI}}. }
#'   \item{imodel}{name of retention & conductivity model: "CH" for
#'   Clapp/Hornberger, "MvG" for Mualem/van Genuchten} \item{root_method}{method
#'   name of the root length density depth distribution function. Any of the
#'   names accepted by \code{\link{makeRootden}} are allowed. Additionally,
#'   \code{'soilvar'} can be used if the root length density depth distribution
#'   is specified in column 'rootden' in the \code{soil}-data.frame}
#' }
#'
#' @export
#'
#' @examples
#' # Default options
#' options_b90 <- setoptions_LWFB90()
#' # Include specific options
#' options_b90_dynamic_phenology <- setoptions_LWFB90(budburst_method = 'Menzel',
#' leaffall_method ='vonWilpert')

setoptions_LWFB90 <- function(...) {
  ctrl <- list(startdate = as.Date("2002-1-1"),
               enddate = as.Date("2003-12-31"),
               fornetrad = "globrad", #"sunhour"
               prec_interval = 1,
               prec_corr = FALSE,
               budburst_method = "fixed",
               leaffall_method = "fixed",
               standprop_input = "parameters", #table
               standprop_interp = "constant", #linear
               use_growthperiod = FALSE, #linear
               lai_method = "b90",
               imodel = "MvG",
               root_method = "betamodel"
               )

  dots <- list(...)

  if (length(dots) > 0 ) {
    if (length(dots[which(names(dots) %in% names(ctrl))]) < length(dots)) {
      warning(paste("Not all arguments found in list! Check names:",
                    names(dots[which(!names(dots) %in% names(ctrl))])
      ))
    }
    ctrl[match(names(dots),names(ctrl))] <- dots
  }
  return(ctrl)

}
