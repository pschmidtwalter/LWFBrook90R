#' Create a list of model control options
#'
#' @param ... named values to be included in return value
#' @return Returns a list of default model control options for use as 'options.b90'-argument in \code{\link{Run.B90}}.
#' @details
#' \describe{
#'   \item{startdate}{start date of the simulation.}
#'   \item{enddate}{end date of the simulation.}
#'   \item{fornetrad}{use global solar radiation (="globrad") or sunshine duration
#'   hours (="sunhour") for net radiation calculation?}
#'   \item{prec.interval}{number of precipitation intervals per day (default is 1).
#'   If prec.interval > 1, a separate file ("in/PRFILE.DAT") has to be provided manually!}
#'   \item{richter.prec.corr}{correct precipitation data for wind and evaporation losses (not implented yet)}
#'   \item{budburst.method}{name of method for budburst calculation. If
#'   'constant' or 'fixed', budburst day of year from parameters is used.
#'   All other methods calculate budburst day of year dynamically from temperatures, and
#'   the method name is passed to the 'start.method'-argument of  \code{\link[vegperiod]{vegperiod}}.}
#'   \item{leaffall.method}{name of method for leaffall calculation. If
#'   'constant' or 'fixed', beginning of leaffall (day of year) from parameters is used.
#'   All other methods calculate budburst day of year dynamically from temperatures, and
#'   the method name is passed to the 'end.method'-argument of  \code{\link[vegperiod]{vegperiod}}.}
#'   \item{standprop.input}{name of input for longterm (interannual) plant development.
#'   'parameters': yearly values of stand properties height, sai, densef, lai come from
#'   parameters, 'table':  values come from a table, see 'longtermdev'-argument of \code{\link{Run.B90}.}}
#'   \item{standprop.interp}{interpolation method for aboveground stand properties.
#'   'linear' or 'constant', see 'approx.method'-argument of \code{\link{approx_standprop}}.}
#'   \item{standprop.use_growthperiod}{Should yearly changes of stand properties (growth)
#'   only take place during the growth period? If TRUE, linear interpolation of height, sai, densef and age are made from budburst until leaffall.
#'   During winter values are constant. Beginning and end of the growth period are taken from parameters budburstdoy and leaffalldoy.
#'   See 'use_growthperiod'-argument of \code{\link{approx_standprop}}.}
#'   \item{lai.method}{name of method for constructing seasonal course leaf area index development from parameters.
#'   Passed to 'method'-argument of \code{\link{MakeSeasLAI}}. }
#'   \item{imodel}{name of hydraulic parameterization: "CH" for Clapp/Hornberger, "MvG" for Mualem/van Genuchten}
#'   \item{root.method}{method name of the root length density depth distribution function. Any of the names accepted by \code{\link{MakeRelRootDens}} are allowed.
#'   Additionally, "soilvar" can be used if the root length density depth distribution is specified in column 'rootden' in the soil-data.frame}
#' }
#' @examples
#' # Default options
#' options.b90 <- setoptions_LWFB90()
#' # Include specific options
#' options.b90_dynamic_phenology <- setoptions_LWFB90(budburst.method = 'Menzel', leaffall.method ='vonWilpert')
#' @export
setoptions_LWFB90 <- function(...) {
  ctrl <- list(startdate = as.Date("2002-1-1"),
               enddate = as.Date("2003-12-31"),
               fornetrad = "globrad", #"sunhour"
               prec.interval = 1,
               richter.prec.corr = FALSE,
               budburst.method = "fixed", #any of the names accepted by 'start.method'-argument of vegperiod::vegperiod()
               leaffall.method = "fixed", #any of the names accepted by 'start.method'-argument of vegperiod::vegperiod()
               standprop.input = "parameters", #table
               standprop.interp = "constant", #linear
               standprop.use_growthperiod = FALSE, #linear
               lai.method = "b90", #any of the names accepted by 'method'-argument of MakeSeasLAI()
               imodel = "MvG", # the parameterization of retention & conductivity function. CH = Clapp-Hornberger, MvG: Mualem van Genuchten
               root.method = "betamodel" #any of the names accepted by the MakeRoots()
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
