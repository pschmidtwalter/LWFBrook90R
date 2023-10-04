#' Run the LWF-Brook90 hydrologic model
#'
#' Sets up the input objects for the LWF-Brook90 hydrologic model, starts the
#' model, and returns the selected results.
#'
#' @param options_b90 Named list of model control options. Use
#'   \code{\link{set_optionsLWFB90}} to generate a list with default model
#'   control options.
#' @param param_b90 Named list of model input parameters. Use
#'   \code{\link{set_paramLWFB90}} to generate a list with default model
#'   parameters.
#' @param climate Data.frame with daily climatic data, or a function that
#'   returns a suitable data.frame. See details for the required variables.
#' @param precip Data.frame with columns 'dates' and 'prec' to supply
#'   precipitation data separately from climate data. Can be used to provide
#'   sub-day resolution precipitation data to LWFBrook90. For each day in dates,
#'   1 (daily resolution) to 240 values of precipitation can be provided, with
#'   the number of values per day defined in \code{options_b90$prec_interval}.
#' @param soil Data.frame containing the hydraulic properties of the soil
#'   layers. See section 'Soil parameters'
#' @param output_fun A function or a list of functions of the form
#'   \code{f(x,...)}, where \code{x} is the object regularly returned by
#'   \code{run_LWFB90}. During function evaluation, \code{x} contains model
#'   input and selected output objects, irrespective of \code{rtrn_input} and
#'   \code{rtrn_output}. Can be used to  aggregate output on-the-fly, and is
#'   especially useful if the function is evaluated within a large multi-run
#'   application, for which the output might overload the memory. (see
#'   \code{\link{run_multi_LWFB90}} and \code{\link{run_multisite_LWFB90}}).
#' @param rtrn_input Logical: append \code{param_b90}, \code{options_b90}, and
#'   daily plant properties (\code{standprop_daily}, as derived from parameters)
#'   to the result?
#' @param rtrn_output Logical: return the simulation results select via
#'   \code{output}?
#' @param chk_input Logical wether to check \code{param_b90},
#'   \code{options_b90}, \code{climate}, \code{precip}, and \code{soil} for
#'   completeness and consistency.
#' @param run Logical: run LWF-Brook90 or only return model input objects?
#'   Useful to inspect the effects of options and parameters on model input.
#'   Default is TRUE.
#' @param timelimit Integer to set elapsed time limits (seconds) for running LWF-Brook90.
#' @param verbose Logical: print messages to the console? Default is FALSE.
#' @param ... Additional arguments passed to \code{output_fun} and/or
#'   \code{climate}, if the latter is a function.
#'
#' @return A list containing the selected model output (if \code{rtrn_output ==
#'   TRUE}), the model input (if \code{rtrn_input == TRUE}, except for
#'   \code{climate}), and the return values of \code{output_fun} if specified.
#'
#'@section Climate input data: The \code{climate} data.frame (or function) must
#'  contain (return) the following variables in columns named 'dates' (Date),
#'  'tmax' (deg C), 'tmin' (deg C), 'tmean' (deg C), 'windspeed' (m/s), 'prec'
#'  (mm) , 'vappres' (kPa), and either 'globrad' ( MJ/(m²d) ) or 'sunhours' (h).
#'  When using \code{sunhours}, please set \code{options_b90$fornetrad =
#'  'sunhours'}.
#'
#'@section Soil input parameters: Each row of \code{soil} represents one layer,
#'  containing the layers' boundaries and soil hydraulic parameters. The column
#'  names for the upper and lower layer boundaries are 'upper' and 'lower' (m,
#'  negative downwards). When using \code{options_b90$imodel = 'MvG'}, the
#'  hydraulic parameters are 'ths', 'thr', 'alpha' (1/m), 'npar', 'ksat' (mm/d)
#'  and 'tort'.  With \code{options_b90$imodel = 'CH'}, the parameters are
#'  'thsat', 'thetaf', 'psif' (kPa), 'bexp', 'kf' (mm/d), and 'wetinf'. For both
#'  parameterizations, the volume fraction of stones has to be named 'gravel'.
#'  If the \code{soil} argument is not provided, list items \code{soil_nodes}
#'  and \code{soil_materials} of \code{param_b90} are used for the simulation.
#'  These have to be set up in advance, see \code{\link{soil_to_param}}.
#'
#' @section Outputs:
#' \tabular{llcl}{
#' \strong{Name} \tab \strong{Description} \tab \strong{Unit} \cr
#' yr \tab year \tab - \cr
#' mo \tab month \tab - \cr
#' da \tab day of month \tab - \cr
#' doy \tab day of year \tab - \cr
#' aa \tab average available energy above canopy \tab W/m2 \cr
#' adef \tab available water deficit in root zone \tab mm \cr
#' asubs \tab average available energy below canopy \tab W/m2 \cr
#' awat \tab total available soil water in layers with roots between -6.18 kPa and \code{param_b90$psicr} \tab mm \cr
#' balerr \tab error in water balance (daily value, output at the day's last precipitation interval) \tab mm \cr
#' byfl \tab total bypass flow \tab mm/d \cr
#' dsfl \tab downslope flow \tab mm/d \cr
#' evap \tab evapotranspiration \tab mm/d \cr
#' flow \tab total streamflow \tab mm/d \cr
#' gwat \tab groundwater storage below soil layers \tab mm \cr
#' gwfl \tab groundwater flow \tab mm/d \cr
#' intr \tab intercepted rain \tab mm \cr
#' ints \tab intercepted snow \tab mm \cr
#' irvp \tab evaporation of intercepted rain \tab mm/d \cr
#' isvp \tab evaporation of intercepted snow \tab mm/d \cr
#' lngnet \tab net longwave radiation \tab W/m2 \cr
#' nits \tab total number of iterations \tab - \cr
#' pint \tab potential interception for a canopy always wet \tab mm/d \cr
#' pslvp \tab potential soil evaporation \tab mm/d \cr
#' ptran \tab potential transpiration \tab mm/d \cr
#' relawat \tab relative available soil water in layers with roots \tab - \cr
#' rfal \tab rainfall \tab mm/d \cr
#' rint \tab rain interception catch rate \tab mm/d \cr
#' rnet \tab rainfall to soil surface \tab mm/d \cr
#' rsno \tab rain on snow \tab mm/d \cr
#' rthr \tab rain throughfall rate \tab mm/d \cr
#' sthr \tab snow throughfall rate \tab mm/d \cr
#' safrac \tab source area fraction \tab - \cr
#' seep \tab seepage loss \tab mm/d \cr
#' sfal \tab snowfall \tab mm/d \cr
#' sint \tab snow interception catch rate \tab mm/d \cr
#' slfl \tab input to soil surface \tab mm/d \cr
#' slvp \tab evaporation rate from soil \tab mm/d \cr
#' slrad \tab average solar radiation on slope over daytime \tab W/m2 \cr
#' solnet \tab net solar radiation on slope over daytime \tab W/m2 \cr
#' smlt \tab snowmelt \tab mm/d \cr
#' snow \tab snowpack water equivalent \tab mm \cr
#' snvp \tab evaporation from snowpack \tab mm/d \cr
#' srfl \tab source area flow \tab mm/d \cr
#' stres \tab tran / ptran (daily value, output at the day's last precipitation interval) \tab - \cr
#' swat \tab total soil water in all layers\tab mm \cr
#' tran \tab transpiration \tab mm/d \cr
#' vrfln \tab vertical matrix drainage from lowest layer \tab mm/d \cr
#'}
#'
#' @section Layer outputs:
#' \tabular{llcl}{
#' \strong{Name} \tab \strong{Description} \tab \strong{Unit} \cr
#' yr \tab year \tab - \cr
#' mo \tab month \tab - \cr
#' da \tab day of month \tab - \cr
#' doy \tab day of year \tab - \cr
#' nl \tab index of soil layer \tab \cr
#' swati \tab soil water volume in layer \tab mm \cr
#' theta \tab water content of soil layer, mm water / mm soil matrix \tab - \cr
#' wetnes \tab wetness of soil layer, fraction of saturation \tab - \cr
#' psimi \tab matric soil water potential for soil layer \tab kPa \cr
#' infl \tab infiltration to soil water in soil layer \tab mm/d \cr
#' byfl \tab bypass flow from soil layer \tab mm/d \cr
#' tran \tab transpiration from soil layer \tab mm/d \cr
#' vrfl \tab vertical matrix drainage from soil layer \tab mm/d \cr
#' dsfl \tab downslope drainage from layer \tab mm/d \cr
#' ntfl \tab net flow into soil layer \tab mm/d \cr
#'}
#'
#' @export
#' @example inst/examples/run_LWFB90-help.R
run_LWFB90 <- function(options_b90,
                       param_b90,
                       climate,
                       precip = NULL,
                       soil = NULL,
                       output_fun = NULL,
                       rtrn_input = TRUE,
                       rtrn_output = TRUE,
                       chk_input = TRUE,
                       run = TRUE,
                       timelimit = Inf,
                       verbose = FALSE,
                       ...) {

  xfunargs <- list(...)

  if (is.function(climate)) {
    if (verbose == TRUE) {
      message("Applying climate input function")
    }
    climfunargsnms <- match.arg(methods::formalArgs(climate), names(xfunargs),several.ok = TRUE)
    climate <- do.call(climate,xfunargs[climfunargsnms])
  }

  # input checks -----
  if (chk_input) {
    chk_options()
    chk_param()
    chk_clim()
    chk_soil()
  }

  # Simulation period ----
  climyears <-  unique(as.integer(format(climate$dates,"%Y")))
  simyears <- seq(from = as.integer(format(options_b90$startdate,"%Y")),
                  to = as.integer(format(options_b90$enddate,"%Y")),
                  by = 1)

  ## Number of Simulation days
  param_b90$ndays <-  as.integer(difftime(options_b90$enddate,options_b90$startdate)) + 1


  ## Vegetation-Period: calculate budburst and leaffall days of year  ----
  budburst_leaffall <- calc_vegperiod(dates = climate$dates, tavg = climate$tmean,
                                      out_yrs = simyears,
                                      budburst_method = options_b90$budburst_method,
                                      leaffall_method = options_b90$leaffall_method,
                                      budburstdoy.fixed = param_b90$budburstdoy,
                                      leaffalldoy.fixed = param_b90$leaffalldoy,
                                      species = param_b90$budburst_species,
                                      est.prev = ifelse(length(climyears) <= 5,
                                                        length(climyears) - 1, 5))
  param_b90$budburstdoy <- budburst_leaffall$start
  param_b90$leaffalldoy <- budburst_leaffall$end

  # Vegetation ----
  # Prepare vegetation parameters
  if (tolower(options_b90$standprop_input) == "table") {
    if (verbose == TRUE) {message("Creating long term stand dynamics from table 'standprop_table'...")}
    param_b90 <- standprop_yearly_to_param(param_b90$standprop_table,
                                           param_b90,
                                           out_yrs = simyears)
  } else {
    if (verbose == TRUE) {message("Creating stand properties from parameters...")}
    # derive age from age_ini for simyears
    param_b90$age <- seq(from = param_b90$age_ini + 1,
                         by = 1, length.out = length(simyears))

  }

  ## Create daily standproperties from parameters ----
  standprop_daily <- make_standprop(options_b90, param_b90, out_yrs = simyears)

  ### constrain to simulation period
  standprop_daily <- standprop_daily[which(standprop_daily$dates >= options_b90$startdate
                                           & standprop_daily$dates <= options_b90$enddate),]
  if (verbose == TRUE) {
    message("Standproperties created succesfully")
  }

  # Prepare climate ----
  # constrain data to simulation period
  climate <- climate[which(climate$dates >= options_b90$startdate
                           & climate$dates <= options_b90$enddate),]
  if (!is.null(precip)){
    precip <- precip[which(precip$dates >= options_b90$startdate
                           & precip$dates <= options_b90$enddate),]
  }

  ## Precipitation correction (Richter) ----
  if (options_b90$correct_prec == TRUE) {
    if (!is.null(precip)) {
      warning("Correction of precipitation not possible for sub-daily precipitation data! Doing nothing.")
    }
    climate$prec <- with(climate, correct_prec(mo, tmean, prec,
                                               station.exposure = param_b90$prec_corr_statexp))
  }

  ## Calculate global radiation from sunshine duration ----
  if (options_b90$fornetrad == "sunhours") {
    climate$globrad <- calc_globrad(climate$dates, climate$sunhours,
                                    param_b90$coords_y)
  }

  ## Make soilnodes & soil materials ----
  # soil provided as argument -> create soil nodes and materials and add them to param_b90
  if (!is.null(soil)) {
    param_b90[c("soil_nodes","soil_materials" )] <- soil_to_param(soil, options_b90$imodel)
  }

  if (options_b90$imodel == "MvG") {
    param_b90$soil_materials <- param_b90$soil_materials[,c("mat","ths","thr","alpha","npar","ksat","tort","gravel")]
  } else {
    param_b90$soil_materials <- param_b90$soil_materials[,c("mat","thsat","thetaf","psif","bexp","kf","wetinf","gravel")]
  }

  ### add initial water potential from parameters
  param_b90$soil_nodes$psiini <- param_b90$psiini

  ## Make Roots -----
  if (options_b90$root_method != "soilvar") {
    param_b90$soil_nodes$rootden <- make_rootden(soilnodes = c(max(param_b90$soil_nodes$upper),
                                                               param_b90$soil_nodes$lower),
                                                 maxrootdepth = param_b90$maxrootdepth,
                                                 method = options_b90$root_method,
                                                 beta = param_b90$betaroot,
                                                 rootdat = param_b90$rootden_table)
  } else {
    if (!is.null(soil)) {
      param_b90$soil_nodes$rootden <- soil$rootden
    } else {
      stopifnot(!is.null(param_b90$soil_nodes$rootden))
    }
  }

  # Execute LWF-Brook90  ----
  if (run) {

    if (verbose == TRUE) {
      message("Running model..." )
    }

    start <- Sys.time()

    simout <- r_lwfbrook90(
      siteparam = data.frame(simyears[1],
                             as.integer(format(options_b90$startdate, "%j")),
                             param_b90$coords_y, param_b90$snowini, param_b90$gwatini,
                             options_b90$prec_interval),
      climveg = cbind(climate[, c("yr", "mo", "da","globrad","tmax","tmin",
                                  "vappres","windspeed","prec","mesfl")],
                      standprop_daily[, c("densef", "height", "lai", "sai", "age")]),
      precdat = precip[,c("yr", "mo", "da","ii","prec", "mesfl")],
      param = param_to_rlwfbrook90(param_b90, options_b90$imodel),
      pdur = param_b90$pdur,
      soil_materials = param_b90$soil_materials,
      soil_nodes = param_b90$soil_nodes[,c("layer","midpoint", "thick", "mat", "psiini", "rootden")],
      output_log = verbose,
      chk_input = chk_input,
      timelimit = timelimit
    )

    finishing_time <- Sys.time()
    simtime <- finishing_time - start
    units(simtime) <- "secs"

    if (verbose == TRUE) {
      message(paste("Simulation successful! Duration:", round(simtime,2), "seconds"))
    }

    ## initialize return value ----
    simres <- list(simulation_duration = simtime,
                   finishing_time = finishing_time)

    ## append model input ----
    # might be needed for access from output_fun. Will be removed again later, if not required
    simres$model_input <- list(options_b90 = options_b90,
                               param_b90 = param_b90,
                               standprop_daily = standprop_daily)


    ## append simulation results  ----
    simres[names(simout)[-1]] <- simout[-1] # append without error-code

    ## apply functions on simulation output ----
    if (!is.null(output_fun)) {
      if (verbose == TRUE) {
        message("Applying custom functions on simulation output...")
      }

      if (!is.list(output_fun)){
        output_fun <- list(output_fun)
      }

      outfunargs <- list(x = simres, ...)

      outfunargsnms <- lapply(output_fun, FUN = function(x,argsnms) {
        match.arg(methods::formalArgs(x),
                  argsnms,
                  several.ok = TRUE)},
        argsnms = names(outfunargs))

      # TODO: simres is copied for use in each output_fun.
      # Better to name -object directly in the call to output_fun,
      # instead of addressing the whole list x?
      simres$output_fun <- tryCatch( {

        Map(do.call, output_fun, lapply(outfunargsnms, function(x,args) args[x], args = outfunargs))

      },
      warning = function(wrn){return(wrn)},
      error = function(err){return(err)})
    }

    ## remove the basic results again if they are not required ----
    if (!rtrn_output) {
      simres <- simres[-which(names(simres) %in% c("output", "layer_output"))]
    }

    ## remove the model_input if not required ----
    if (!rtrn_input) {
      simres <- simres[-which(names(simres) == "model_input")]
    }

  } else {
    # 'dry' run = FALSE -> always return model input
    return(list(options_b90 = options_b90,
                param_b90 = param_b90,
                standprop_daily = standprop_daily))
  }

  if (verbose == TRUE) {
    message("Finished!")
  }
  return(simres)
}

## check-functions for input ---------------------------------------------------

chk_options <- function(){
  eval.parent(quote({ # manipulate the calling environment
    names(options_b90) <- tolower(names(options_b90))

    stopifnot(all(names(options_b90) %in% c("startdate","enddate","fornetrad","prec_interval",
                                            "correct_prec","budburst_method",
                                            "leaffall_method", "standprop_input", "standprop_interp",
                                            "use_growthperiod","lai_method","imodel", "root_method")))

    options_b90$fornetrad <- match.arg(options_b90$fornetrad, choices = c("globrad","sunhours"))
    options_b90$standprop_input <- match.arg(options_b90$standprop_input, choices = c("parameters", "table"))
    options_b90$lai_method <- match.arg(options_b90$lai_method, choices = c("b90", "linear", "Coupmodel"))
    options_b90$root_method <- match.arg(options_b90$root_method, choices = c("betamodel", "table", "linear", "constant", "soilvar"))
    options_b90$imodel <- match.arg(options_b90$imodel, choices = c("MvG", "CH"))

    if (!inherits(options_b90$startdate, "Date")) {
      stop("Please provide 'options_b90$startdate' as Date-object")}
    if (!inherits(options_b90$enddate, "Date")) {
      stop("Please provide 'options_b90$enddate' as Date-object")}
    if (!(options_b90$startdate < options_b90$enddate)) {
      stop("Check options_b90: 'startdate > enddate ")}

    if (options_b90$budburst_method %in% c("const", "const.", "constant")) options_b90$budburst_method <- "fixed"
    if (options_b90$leaffall_method %in% c("const", "const.", "constant")) options_b90$leaffall_method <- "fixed"
  }))
}

chk_param <- function() {
  eval.parent(quote({ # manipulate the calling environment
    names(param_b90) <- tolower(names(param_b90))
    nms <- c("maxlai","sai","sai_ini","height","height_ini","densef",
             "densef_ini","age_ini","winlaifrac","budburst_species","budburstdoy",
             "leaffalldoy","shp_budburst","shp_leaffall","shp_optdoy","emergedur","leaffalldur",
             "lai_doy","lai_frac","alb","albsn","ksnvp","fxylem",
             "mxkpl","lwidth","psicr","nooutf","lpc","cs",
             "czs","czr","hs","hr","rhotp","nn",
             "maxrlen","initrlen","initrdep","rrad","rgrorate","rgroper",
             "maxrootdepth","betaroot","radex","glmax","glmin",
             "rm","r5","cvpd","tl","t1","t2",
             "th","frintlai","frintsai","fsintlai","fsintsai","cintrl",
             "cintrs","cintsl","cintss","infexp","bypar","qfpar",
             "qffc","imperv","drain","gsc","gsp","ilayer",
             "qlayer","z0s","rstemp","melfac","ccfac","laimlt",
             "saimlt","grdmlt","maxlqf","snoden","obsheight","rssa",
             "rssb","dtimax","dswmax","dpsimax",
             "wndrat","fetch","z0w","zw","zminh","coords_x",
             "coords_y","c1","c2","c3","pdur","eslope",
             "aspect","dslope","slopelen","intrainini","intsnowini","gwatini",
             "snowini","psiini")
    if ( any(!nms %in% names(param_b90) )) {
      stop(paste("param_b90-list is incomplete. Missing list items:",
                 paste(nms[which(!nms %in% names(param_b90))], collapse = ", ")))
    }
  }))

}

chk_clim <- function() {

  eval.parent(quote({

    # climate name checks
    names(climate) <- tolower(names(climate))

    if (!all(c("dates", "tmax", "tmin", options_b90$fornetrad, "vappres", "windspeed") %in% tolower(names(climate)))) {
      if (is.null(precip)) {}
      stop("Please check column names of 'climate'. Must contain: \n
           'dates', 'tmax', 'tmin', 'vappres', 'windspeed', and 'globrad' (or 'sunhours') ")
    }

    stopifnot(inherits(climate$dates, "Date"))
    if (min(climate$dates) > options_b90$startdate | max(climate$dates) < options_b90$enddate){
      stop("climate not covering requested simulation period completely.")
    }

    # Climate
    # if (options_b90$fornetrad == "globrad" & !any( names(climate) == "globrad" )) {
    #   if (any( names(climate) == "sunhour" )) {
    #     options_b90$fornetrad <- "sunhour"
    #     warning("Global radiation missing, will be calculated from sunshine duration!")
    #   } else {
    #     stop("Please either provide globrad or sunhour with your climate data!")
    #   }
    # } else {
    #   if (any( names(climate) == "globrad" )) {
    #     options_b90$fornetrad <- "globrad"
    #     stop("Please either provide 'globrad' or 'sunhours' with your climate!")}
    # }

    if (any(!c("yr", "mo", "da") %in% names(climate))) {
      climate$yr <- data.table::year(climate$dates)
      climate$mo <- data.table::month(climate$dates)
      climate$da <- data.table::mday(climate$dates)
    }

    if (!any( names(climate) == "mesfl") ) {
      climate$mesfl <- 0
    }

    if (is.null(precip) ){
      stopifnot("prec" %in% names(climate))

    } else {
      names(precip) <- tolower(names(precip))
      stopifnot(all(c("dates","prec") %in% tolower(names(precip))))
      if (!any( names(precip) == "mesfl") ) {
        precip$mesfl <- 0
      }
      if (nrow(climate)*options_b90$prec_interval != nrow(precip)) {
        stop("Climate and Precipitation data provided do not fit to the precipitation
             interval defined in options_b90$prec_interval.")
      } else {
        if (options_b90$prec_interval == 1) {
          climate$prec <- precip$prec
          precip <- NULL
        } else {
          precip$ii <- rep(1:options_b90$prec_interval,nrow(climate))
          precip$yr <- data.table::year(precip$dates)
          precip$mo <- data.table::month(precip$dates)
          precip$da <- data.table::mday(precip$dates)
          climate$prec <- -999
        }
      }
    }
  }))
}

chk_soil <- function(){

  eval.parent(quote({
    # soil names
    if (is.null(soil)) {
      if (is.null(param_b90$soil_nodes) | is.null(param_b90$soil_materials)) {
        stop("Please provide soil parameters as items 'soil_nodes' and 'soil_materials' via 'param_b90',
           when not using 'soil'-argument in run_LWFB90.")
      }

      names(param_b90$soil_nodes) <- tolower(names(param_b90$soil_nodes))
      names(param_b90$soil_materials) <- tolower(names(param_b90$soil_materials))

      stopifnot(all(c( "upper", "lower", "mat") %in% names(param_b90$soil_nodes)))
      if (anyNA(param_b90$soil_nodes[,c( "upper", "lower", "mat")])) {
        stop("No NAs allowed in param_b90$soil_nodes")
      }

      if (options_b90$imodel == "MvG" ) {
        stopifnot(all(c("mat","ths","thr","alpha","npar","ksat","tort","gravel") %in% names(param_b90$soil_materials)))
        if (anyNA(param_b90$soil_materials[,c("mat","ths","thr","alpha","npar","ksat","tort","gravel")])) {
          stop("No NAs allowed in param_b90$soil_materials!")
        }

      } else {
        stopifnot(all(c("mat","thsat","thetaf","psif","bexp","kf","wetinf","gravel") %in% names(param_b90$soil_materials)))
        if (anyNA(param_b90$soil_materials[,c("mat","thsat","thetaf","psif","bexp","kf","wetinf","gravel")])) {
          stop("No NAs allowed in param_b90$soil_materials!")
          }
      }

      if (options_b90$root_method == "soilvar" & is.null(param_b90$soil_nodes$rootden)) {
        stop("Please provide column 'rootden' in param_b90$soil_nodes when using options_b90$root_method = 'soilvar'.")
      }


    } else {
      names(soil) <- tolower(names(soil))
      if (options_b90$imodel == "MvG") {
        stopifnot(all(c("upper","lower", "ths","thr","alpha","npar","ksat","tort","gravel") %in% names(soil)))
        if (anyNA(soil[,c("upper","lower","ths","thr","alpha","npar","ksat","tort","gravel")])) {
          stop("No NAs allowed in 'soil'!")
        }
      } else {
        stopifnot(all(c("upper","lower", "thsat","thetaf","psif","bexp","kf","wetinf","gravel") %in% names(soil)))
        if (anyNA(soil[,c("upper","lower","thsat","thetaf","psif","bexp","kf","wetinf","gravel")])) {
          stop("No NAs allowed in 'soil'!")
        }
      }

      if (options_b90$root_method == "soilvar" & is.null(soil$rootden)) {
        stop("Please provide column 'rootden' in 'soil'-data.frame when using options_b90$root_method = 'soilvar'.")
      }

    }
  }))

}
