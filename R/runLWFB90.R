#' Run the LWF-Brook90 hydrological model
#'
#' Sets up the input objects for the LWF-Brook90 hydrological model, starts the model,
#' and returns the selected results.
#'
#' @param options.b90 Named list of model control options. Use
#' \code{\link{setoptions_LWFB90}} to generate a list with default model control options.
#' @param param.b90 Named list of model input parameters. Use
#' \code{\link{setparam_LWFB90}} to generate a list with default model parameters.
#' @param climate Data.frame with daily climate data. See details for the required variables.
#' @param precip Data.frame with columns 'dates' and 'prec' to supply precipitation data separately from climate data.
#' Can be used to provide sub-day resolution precipitation data to LWFBrook90. For each day in dates,
#' 1 (daily resolution) to 240 values of precipitation can be provided, with the number of values
#' per day defined in \code{options.b90$prec.interval}.
#' @param soil Data.frame containing the hydraulic properties of the soil layers. See section 'Soil parameters'
#' @param output A [7,5]-matrix flagging the desired model output datasets at different time intervals. Use
#' \code{\link{setoutput_LWFB90}} to generate and edit a default output selection matrix. Alternatively,
#' use -1 (the default) to return the raw daily and soil layer outputs.
#' @param output_fun A function or a list of functions to be performed on the output objects selected by \code{output}.
#' Can be used to aggregate output or calculate goodness of fit measures on-the-fly, or to write results instantly to a file or database.
#' Useful if the function is evaluated within a large multi-run application, which might overload the memory.
#' (see \code{\link{mrunLWFB90}} and \code{\link{msiterunLWFB90}}).
#' @param rtrn.input Logical: append 'param.b90', 'options.b90', 'soil' and daily plant
#' properties ('standprop_daily', as derived from parameters) to the result?
#' @param rtrn.output Logical: return the simulation results?
#' @param chk.input Logical wether to check param.b90, options.b90, climate, precip, and soil
#' for completeness and consistency.
#' @param run Logical: run LWF-Brook90 or only return model input objects?
#' Useful to inspect the effects of options and parameters on model input. Default is TRUE.
#' @param timelimit Integer to set elapsed time limits for running LWF-Brook90.
#' @param verbose Logical: print messages to the console? Default is FALSE.
#' @param ... Additional arguments passed to \code{output_fun}.
#'
#' @return A list containing the selected model output, the model input (except for \code{climate}) if desired,
#' and the return values of \code{output_fun} if specified.
#'
#'@section Climate input data:
#' The \code{climate} data.frame must contain the following variables in columns named
#' \code{dates} (Date), \code{tmax} (deg C), \code{tmin} (deg C), \code{tmean} (deg C),
#' \code{wind} (m s-1), \code{prec} (mm) , \code{vappres} (kPa), and either \code{globrad} (MJ d-1 m-2)
#' or \code{sunhours} (hours). When using \code{sunhours}, please set \code{options.b90$fornetrad = 'sunhours'}.
#'
#' @section Soil input parameters:
#' Each row of \code{soil} represents one layer, containing the layers' boundaries and soil hydraulic parameters.
#' The column names for the upper and lower layer boundaries are \code{upper} and \code{lower} (m, negative downwards).
#' When using options.b90$imodel = 'MvG', the hydraulic parameters are  \code{ths}, \code{thr},
#'  \code{alpha} (m-1), \code{npar}, \code{ksat} (mm d-1) and \code{tort}.  With options.b90$imodel = 'CH',
#'  the parameters are \code{thsat}, \code{thetaf}, \code{psif} (kPa), \code{bexp},
#'  \code{kf} (mm d-1), and \code{wetinf}. For both parameterizations, the volume fraction of stones has to be named \code{gravel}.
#'  If the soil data.frame is not provided, list items 'soil_nodes' and 'soil_materials'
#'  of param.b90 are used for the simulation. These have to be set up in advance, see \code{\link{soil_to_param}}.
#'
#' @section Daily outputs:
#' \tabular{llcl}{
#' \strong{Name} \tab \strong{Description} \tab \strong{Unit} \cr
#' adef \tab air deficit in the root zone \tab - \cr
#' awat \tab total available soil water in layers with roots between -6.18 kPa and PSICR \tab mm \cr
#' balerr \tab error in water balance \tab mm \cr
#' byfl \tab total bypass flow \tab mm \cr
#' dsfl \tab downslope flow \tab mm \cr
#' evap \tab evapotranspiration \tab mm \cr
#' flow \tab total streamflow \tab mm \cr
#' gwat \tab groundwater storage below soil layers at the end of the time interval \tab mm \cr
#' gwfl \tab groundwater flow \tab mm \cr
#' intr \tab intercepted rain at the end of the time interval \tab mm \cr
#' ints \tab intercepted snow at the end of the time interval \tab mm \cr
#' irvp \tab evaporation of intercepted rain \tab mm \cr
#' isvp \tab evaporation of intercepted snow \tab mm \cr
#' nits \tab total number of iterations in time interval \tab - \cr
#' pint \tab potential interception for a canopy always wet \tab mm \cr
#' pslvp \tab potential soil evaporation \tab mm \cr
#' ptran \tab potential transpiration \tab mm \cr
#' relawat \tab relative available soil water in layers with roots \tab - \cr
#' rfal \tab rainfall \tab mm \cr
#' rint \tab rain interception \tab mm \cr
#' rnet \tab rainfall to soil surface \tab mm \cr
#' rsno \tab rain on snow \tab mm \cr
#' safrac \tab source area fraction \tab - \cr
#' seep \tab seepage loss \tab mm \cr
#' sfal \tab snowfall \tab mm \cr
#' sint \tab snow interception \tab mm \cr
#' slfl \tab input to soil surface \tab mm \cr
#' slvp \tab evaporation rate from soil \tab mm \cr
#' smlt \tab snowmelt \tab mm \cr
#' snow \tab snowpack water equivalent \tab mm \cr
#' snvp \tab evaporation from snowpack \tab mm \cr
#' srfl \tab source area flow \tab mm \cr
#' stres \tab TRAN / PTRAN for time period \tab - \cr
#' swat \tab total soil water in all layers at the end of the time interval \tab mm \cr
#' tran \tab transpiration \tab mm \cr
#' vrfln \tab vertical matrix drainage from lowest layer \tab mm \cr
#'}
#'
#' @section Layer outputs:
#' \tabular{llcl}{
#' \strong{Name} \tab \strong{Description} \tab \strong{Unit} \cr
#' nl \tab index of soil layer \tab \cr
#' swati \tab soil water volume in layer \tab mm \cr
#' theta \tab water content of soil layer, mm water / mm soil matrix \tab - \cr
#' wetnes \tab wetness of soil layer, fraction of saturation \tab - \cr
#' psimi \tab matric soil water potential for soil layer \tab kPa \cr
#' psiti \tab total soil water potential for a soil layer \tab kPa \cr
#' infl \tab infiltration to soil water in soil layer \tab mm \cr
#' byfl \tab bypass flow from soil layer \tab mm \cr
#' tran \tab transpiration from soil layer \tab mm \cr
#' slvp \tab soil evaporation from a soil layer \tab mm \cr
#' vrfl \tab vertical matrix drainage from soil layer \tab mm \cr
#' dsfl \tab downslope drainage from layer \tab mm \cr
#' ntfl \tab net flow into soil layer \tab mm \cr
#'}
#'
#' @export
#' @example inst/examples/runLWFB90-help.R
runLWFB90 <- function(options.b90,
                      param.b90,
                      climate,
                      precip = NULL,
                      soil = NULL,
                      output = setoutput_LWFB90(),
                      output_fun = NULL,
                      rtrn.input = TRUE,
                      rtrn.output = TRUE,
                      chk.input = TRUE,
                      run = TRUE,
                      timelimit = Inf,
                      verbose = TRUE,
                      ...) {


  # input checks ----------------------#'
  if (chk.input) {
    chk_options()
    chk_param()
    chk_clim()
    chk_soil()
  }

  # ---- Simulation period ----------------------------------------------------------
  climyears <-  unique(as.integer(format(climate$dates,"%Y")))
  simyears <- seq(from = as.integer(format(options.b90$startdate,"%Y")),
                  to = as.integer(format(options.b90$enddate,"%Y")),
                  by = 1)

  # Number of Simulation days
  param.b90$ndays <-  as.integer(difftime(options.b90$enddate,options.b90$startdate)) + 1


  # Vegetation-Period: calculate budburst and leaffall days of year  ----------------
  budburst_leaffall <- calc_vegperiod(dates = climate$dates, tavg = climate$tmean,
                                      out.years = simyears,
                                      budburst.method = options.b90$budburst.method,
                                      leaffall.method = options.b90$leaffall.method,
                                      budburstdoy.fixed = param.b90$budburstdoy,
                                      leaffalldoy.fixed = param.b90$leaffalldoy,
                                      species = param.b90$budburst.species,
                                      est.prev = ifelse(length(climyears) <= 5,
                                                        length(climyears) - 1, 5))
  param.b90$budburstdoy <- budburst_leaffall$start
  param.b90$leaffalldoy <- budburst_leaffall$end

  # ---- Vegetation -----------------------------------------------------------------
  # Prepare vegetation parameters
  if (tolower(options.b90$standprop.input) == "table") {
    if (verbose == T) {message("Creating long term stand dynamics from table 'standprop.table'...")}
    param.b90 <- standprop_yearly_to_param(param.b90$standprop.table,
                                           param.b90,
                                           out.years = simyears)
  } else {
    if (verbose == T) {message("Creating stand properties from parameters...")}
    # derive age from age.ini for simyears
    param.b90$age <- seq(from = param.b90$age.ini + 1,
                         by = 1, length.out = length(simyears))

  }

  # ---- Create daily standproperties from parameters
  standprop_daily <- make_standprop(options.b90, param.b90, out.years = simyears)

  # constrain to simulation period
  standprop_daily <- standprop_daily[which(standprop_daily$dates >= options.b90$startdate
                                           & standprop_daily$dates <= options.b90$enddate),]

  if (verbose == T) {
    message("Standproperties created succesfully")
  }

  # ---- Prepare climate for input----------------------------------------------------

  # constrain data to simulation period
  climate <- climate[which(climate$dates >= options.b90$startdate
                           & climate$dates <= options.b90$enddate),]

  # Precipitation correction (Richter)
  if (options.b90$prec.corr == TRUE) {
    climate$prec <- with(climate, prec_corr(month, tmean, prec,
                                            station.exposure = param.b90$prec.corr.statexp))
  }

  #Calculate global radiation from sunshine duration
  if (options.b90$fornetrad == "sunhours") {
    climate$globrad <- with(climate,
                            calc_globrad( as.integer(format(dates, "%j")),
                                          sunhours, param.b90$coords_y ))
  }

  # ---- Make soilnodes & soil materials --------------------------------------------
  # soil provided as argument -> create soil nodes and materials and add them to param.b90
  if (!is.null(soil)) {
    param.b90[c("soil_nodes","soil_materials" )] <- soil_to_param(soil, options.b90$imodel)
  }

  if (options.b90$imodel == "MvG") {
    param.b90$soil_materials <- param.b90$soil_materials[,c("mat","ths","thr","alpha","npar","ksat","tort","gravel")]
  } else {
    param.b90$soil_materials <- param.b90$soil_materials[,c("mat","thsat","thetaf","psif","bexp","kf","wetinf","gravel")]
  }

  # add initial water potential from parameters
  param.b90$soil_nodes$psiini <- param.b90$psiini

  # Make Roots ----------------------------------------------------------------------
  if (options.b90$root.method != "soilvar") {
    param.b90$soil_nodes$rootden <- MakeRelRootDens(soilnodes = c(max(param.b90$soil_nodes$upper),
                                                                      param.b90$soil_nodes$lower),
                                                    maxrootdepth = param.b90$maxrootdepth,
                                                    method = options.b90$root.method,
                                                    beta = param.b90$betaroot,
                                                    rootdat = param.b90$rootden.tab)
  } else {
    if (!is.null(soil)) {
      param.b90$soil_nodes$rootden <- soil$rootden
    } else {
      stopifnot(!is.null(param.b90$soil_nodes$rootden))
    }
  }

  # ---- Execute LWF-Brook90  -------------------------------------------------------
  if (run) {

    if (verbose == T) {
      message("Running model..." )
    }

    start <- Sys.time()

    simout <- r_lwfbrook90(
      siteparam = data.frame(simyears[1],
                             as.integer(format(options.b90$startdate, "%j")),
                             param.b90$coords_y, param.b90$snowini, param.b90$gwatini,
                             options.b90$prec.interval),
      climveg = cbind(climate[, c("yr", "mo", "da","globrad","tmax","tmin",
                                  "vappres","wind","prec","mesfl")],
                      standprop_daily[, c("densef", "height", "lai", "sai", "age")]),
      precdat = precip,
      param = param_to_rlwfbrook90(param.b90, options.b90$imodel),
      pdur = param.b90$pdur,
      soil_materials = param.b90$soil_materials,
      soil_nodes = param.b90$soil_nodes[,c("layer","midpoint", "thick", "mat", "psiini", "rootden")],
      output_log = verbose,
      timelimit = timelimit
    )

    chk_errors() # check for simulation errors----------------------------------

    finishing_time <- Sys.time()
    simtime <- finishing_time - start
    units(simtime) <- "secs"

    if (verbose == T) {
      message(paste("Simulation successful! Duration:", round(simtime,2), "seconds"))
    }

    # ---- process and manage outputs ---------------------------------------------------------------
    # precipitation interval outputs (to come)

    # daily outputs
    simout$daily_output <- data.table::data.table(simout$daily_output)
    data.table::setnames(simout$daily_output, names(simout$daily_output),
                         c('yr','mo','da','doy','rfal','rint','sfal','sint','rsno',
                           'rnet','smlt','snow','swat','gwat','intr', 'ints','evap','tran','irvp',
                           'isvp','slvp','snvp','pint','ptran','pslvp','flow','seep',
                           'srfl','slfl','byfl','dsfl','gwfl','vrfln','safrac',
                           'stres','adef','awat','relawat','nits','balerr'))

    # layer outputs
    simout$layer_output <- data.table::rbindlist(lapply(seq(dim(simout$layer_output)[3]),
                                                        function(x) data.frame(simout$layer_output[ , , x])),
                                                 idcol = "nl")
    data.table::setnames(simout$layer_output, paste0("X", 1:16),
                         c('yr','mo','da','doy','swati','theta','wetnes','psimi','psiti','infl',
                           'byfl','tran','slvp','vrfl','dsfl','ntfl'))

    simout$layer_output <- simout$layer_output[order(simout$layer_output$yr,
                                                     simout$layer_output$doy,
                                                     simout$layer_output$nl),]

    # ---- initialize return value ---------------------------------------------------------------
    simres <- list(simulation_duration = simtime,
                   finishing_time = finishing_time)

    # ---- append model input -------------------------------------------------------
    # might be needed for access from output_fun. if not required, will be removed again later
    simres$model_input <- list(options.b90 = options.b90,
                               param.b90 = param.b90,
                               standprop_daily = standprop_daily)


    # ---- append simulation results  -------------------------------------------------------
    # either raw output or only data set selections
    if (is.matrix(output) & all(dim(output) == c(7,5))) {
      simres <- c(simres, process_outputs(simout, output))
    } else {
      simres[names(simout)] <- simout
    }

    # ---- apply functions on simulation output -------------------------------
    if (!is.null(output_fun)) {
      if (verbose == T) {
        message("Applying custom functions on simulation output...")
      }

      if (!is.list(output_fun)){
        output_fun <- list(output_fun)
      }

      outfunargs <- list(x = simres,...)

      outfunargsnms <- lapply(output_fun, FUN = function(x,argsnms) {
        match.arg(methods::formalArgs(x),
                  argsnms,
                  several.ok = T)},
        argsnms = names(outfunargs))

      # TODO: simres is copied for use in each output_fun.
      # Better to name output-object (e.g. SWATDAY.ASC) directly in the call to output_fun,
      # instead of adressing the whole list x.
      simres$output_fun <- tryCatch( {

        Map(do.call, output_fun, lapply(outfunargsnms, function(x,args) args[x], args = outfunargs))

      },
      warning = function(wrn){return(wrn)},
      error = function(err){return(err)})
    }

    # remove the basic results again if they are not required
    if (!rtrn.output) {
      if (is.matrix(output) & all(dim(output) == c(7,5))) {
        simres <- simres[-which(grepl(".ASC", names(simres), fixed = T))]
      } else {
        simres <- simres[-which(names(simres) %in% c("daily_output", "layer_output"))]
      }

      # remove the model_input if not required
      if (!rtrn.input) {
        simres <- simres[-which(names(simres) == "model_input")]
      }

    }


  } else {
    # 'dry' run = FALSE -> always return model input
    return(list(options.b90 = options.b90,
                param.b90 = param.b90,
                standprop_daily = standprop_daily))
  }

  if (verbose == T) {
    message("Finished!")
  }
  return(simres)
}

# check for errors -------------------------------------------------------------
chk_errors <- function(){
  eval.parent(quote({
    if (simout$error_code != 0L) {
      if (simout$error_code == 1L) stop("Simulation terminated abnormally: 'initial matrix psi > 0'
                                        (rerun with verbose = TRUE to see more information)")
      if (simout$error_code == 2L) stop("Simulation initialazation failed: 'FWETK failed to determine wetness at KF'
                                           (rerun with verbose = TRUE  to see more information)")
      if (simout$error_code == 3L) stop("Simulation terminated abnormally: 'inconsistent dates in climate!'
                                        (rerun with verbose = TRUE  to see more information)")
      if (simout$error_code == 4L) stop("Simulation terminated abnormally: 'inconsistent dates in precipitation input!'
                                        (rerun with verbose = TRUE  to see more information)")
      if (simout$error_code == 5L) stop("Simulation terminated abnormally: 'wrong precipitation interval input!'
                                        (rerun with verbose = TRUE  to see more information)")
      if (simout$error_code == 6L) stop("Simulation terminated abnormally: 'negative soil water storage!'
                                        (rerun with verbose = TRUE  to see more information)")
      if (simout$error_code == 7L) stop("Simulation terminated abnormally: 'water storage exceeds water capacity!'
                                        (rerun with verbose = TRUE  to see more information)")
    }
  }))

}

## check-functions for input ---------------------------------------------------

chk_options <- function(){
  eval.parent(quote({ # manipulate the calling environment
    names(options.b90) <- tolower(names(options.b90))

    stopifnot(all(names(options.b90) %in% c("startdate","enddate","fornetrad","prec.interval",
                                            "prec.corr","budburst.method",
                                            "leaffall.method", "standprop.input", "standprop.interp",
                                            "standprop.use_growthperiod","lai.method","imodel", "root.method")))

    options.b90$fornetrad <- match.arg(options.b90$fornetrad, choices = c("globrad","sunhours"))
    options.b90$standprop.input <- match.arg(options.b90$standprop.input, choices = c("parameters", "table"))
    options.b90$lai.method <- match.arg(options.b90$lai.method, choices = c("b90", "linear", "Coupmodel"))
    options.b90$root.method <- match.arg(options.b90$root.method, choices = c("betamodel", "table", "linear", "constant", "soilvar"))
    options.b90$imodel <- match.arg(options.b90$imodel, choices = c("MvG", "CH"))

    if (!inherits(options.b90$startdate, "Date")) {
      stop("Please provide 'options.b90$startdate' as Date-object")}
    if (!inherits(options.b90$enddate, "Date")) {
      stop("Please provide 'options.b90$enddate' as Date-object")}
    if (!(options.b90$startdate < options.b90$enddate)) {
      stop("Check options.b90: 'startdate > enddate ")}

    if (options.b90$budburst.method %in% c("const", "const.", "constant")) options.b90$budburst.method <- "fixed"
    if (options.b90$leaffall.method %in% c("const", "const.", "constant")) options.b90$leaffall.method <- "fixed"
  }))
}

chk_param <- function() {
  eval.parent(quote({ # manipulate the calling environment
    names(param.b90) <- tolower(names(param.b90))
    nms <- c("maxlai","sai","sai.ini","height","height.ini","densef",
             "densef.ini","age.ini","winlaifrac","budburst.species","budburstdoy",
             "leaffalldoy","shape.budburst","shape.leaffall","shape.optdoy","emergedur","leaffalldur",
             "lai.doy","lai.frac","alb","albsn","ksnvp","fxylem",
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
    if ( any(!nms %in% names(param.b90) )) {
      stop(paste("param.b90-list is incomplete. Missing list items:",
                 paste(nms[which(!nms %in% names(param.b90))], collapse = ", ")))
    }
  }))

}

chk_clim <- function() {

  eval.parent(quote({

    # climate name checks
    names(climate) <- tolower(names(climate))
    stopifnot(all(c("dates", "tmax", "tmin",options.b90$fornetrad, "vappres", "wind") %in% tolower(names(climate))))
    stopifnot(inherits(climate$dates, "Date"))
    if (min(climate$dates) > options.b90$startdate | max(climate$dates) < options.b90$enddate){
      stop("climate not covering requested simulation period completely.")
    }

    # Climate
    # if (options.b90$fornetrad == "globrad" & !any( names(climate) == "globrad" )) {
    #   if (any( names(climate) == "sunhour" )) {
    #     options.b90$fornetrad <- "sunhour"
    #     warning("Global radiation missing, will be calculated from sunshine duration!")
    #   } else {
    #     stop("Please either provide globrad or sunhour with your climate data!")
    #   }
    # } else {
    #   if (any( names(climate) == "globrad" )) {
    #     options.b90$fornetrad <- "globrad"
    #     stop("Please either provide 'globrad' or 'sunhours' with your climate!")}
    # }


    if (any(!c("yr", "mo", "da") %in% names(climate))) {
      climate$yr <- as.integer(format(climate$dates, "%Y"))
      climate$mo <- as.integer(format(climate$dates, "%m"))
      climate$da <- as.integer(format(climate$dates, "%d"))
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
      if (nrow(climate)*options.b90$prec.interval != nrow(precip)) {
        stop("Climate and Precipitation data provided do not fit to the precipitation
             interval defined in options.b90$prec.interval.")
      } else {
        if (options.b90$prec.interval == 1) {
          climate$prec <- precip$prec
          precip <- NULL
        } else {
          precip$ii <- rep(1:options.b90$prec.interval,nrow(climate))
          precip$yr <- as.integer(format(precip$dates, "%Y"))
          precip$mo <- as.integer(format(precip$dates, "%m"))
          precip$da <- as.integer(format(precip$dates, "%d"))
          precip <- precip[, c("yr","mo","da", "ii", "prec", "mesfl")]
          climate$prec <- -999
        }
      }
    }
  }))
}

# chk_obs <- function(){
#   eval.parent(quote({
#     if (!is.null(obs)) {
#       names(obs) <- tolower(names(obs))
#       stopifnot("dates" %in% names(obs),
#                 inherits(obs$dates, "Date"),
#                 length(obs) > 1)
#       if (min(obs$dates) > options.b90$startdate & max(obs$dates) < options.b90$enddate) {
#         stop("Your observations are not within the simulation period.")
#       }
#       if (is.null(gof_fun)) {
#         stop("Please provide a function(sim, obs) or list of functions to calculate
#            goodness-of-fit measures for observed variables.")
#       }
#     }
#   }))
# }

chk_soil <- function(){

  eval.parent(quote({
    # soil names
    if (is.null(soil)) {
      if (is.null(param.b90$soil_nodes) | is.null(param.b90$soil_materials)) {
        stop("Please provide soil parameters as items 'soil_nodes' and 'soil_materials' via 'param.b90',
           when not using 'soil'-argument in runLWFB90.")
      }

      names(param.b90$soil_nodes) <- tolower(names(param.b90$soil_nodes))
      names(param.b90$soil_materials) <- tolower(names(param.b90$soil_materials))

      stopifnot(all(c( "upper", "lower", "mat") %in% names(param.b90$soil_nodes)))
      if (options.b90$imodel == "MvG" ) {
        stopifnot(all(c("mat","ths","thr","alpha","npar","ksat","tort","gravel") %in% names(param.b90$soil_materials)))
      } else {
        stopifnot(all(c("mat","thsat","thetaf","psif","bexp","kf","wetinf","gravel") %in% names(param.b90$soil_materials)))
      }
      if (options.b90$root.method == "soilvar" & is.null(param.b90$soil_nodes$rootden)) {
        stop("Please provide column 'rootden' in param.b90$soil_nodes when using options.b90$root.method = 'soilvar'.")
      }
    } else {
      names(soil) <- tolower(names(soil))
      if (options.b90$imodel == "MvG") {
        stopifnot(all(c("upper","lower", "ths","thr","alpha","npar","ksat","tort","gravel") %in% names(soil)))
      } else {
        stopifnot(all(c("upper","lower", "thsat","thetaf","psif","bexp","kf","wetinf","gravel") %in% names(soil)))
      }

      if (options.b90$root.method == "soilvar" & is.null(soil$rootden)) {
        stop("Please provide column 'rootden' in 'soil'-data.frame when using options.b90$root.method = 'soilvar'.")
      }
    }
  }))

}

# output processing ------------------------------------------------------------
process_outputs <- function(simout, output) {

  # to pass CRAN check notes
  adef<-NULL;awat<-NULL;balerr<-NULL;da<-NULL;doy<-NULL;evap<-NULL;flow<-NULL;gwat<-NULL;
  intr<-NULL;ints<-NULL;mo<-NULL;nits<-NULL;nl<-NULL;relawat<-NULL;rfal<-NULL;safrac<-NULL;
  seep<-NULL;sfal<-NULL;snow<-NULL;stres<-NULL;swat<-NULL;vrfln<-NULL;yr<-NULL;

  selection <- rownames(output)[which(rowSums(output) > 0)]

  if (any(selection == "Budg")) {
    Budg <- simout$daily_output[,c("yr","mo","da","doy","rfal","sfal","flow", "evap", "seep","snow","swat","gwat","intr","ints")]}
  if (any(selection == "Flow")){
    Flow <- simout$daily_output[,c("yr","mo","da","doy","flow","seep","srfl","slfl","byfl","dsfl","gwfl","vrfln")]}
  if (any(selection == "Evap")){
    Evap <- simout$daily_output[,c("yr","mo","da","doy","flow","evap","tran","irvp","isvp","slvp","snvp","pint","ptran","pslvp")]}
  if (any(selection == "Abov")){
    Abov <- simout$daily_output[,c("yr","mo","da","doy","rfal","rint","sfal","sint","rsno","rnet","smlt","slfl","srfl")]}
  if (any(selection == "Belo")){
    Belo <- simout$layer_output[,c("yr","mo","da","doy","nl","infl","byfl","tran","slvp","vrfl","dsfl","ntfl")]}
  if (any(selection == "Swat")){
    Swat <- simout$layer_output[,c("yr","mo","da","doy","nl","swati","theta","wetnes","psimi","psiti")]}
  if (any(selection == "Misc")){
    Misc <- simout$daily_output[,c("yr","mo","da","doy","vrfln","safrac","stres","adef","awat","relawat","nits","balerr")]}

  moutputs <- list() # results collection

  for (sel in selection) {
    X <- get(sel)
    if (sel  %in% c("Flow", "Evap", "Abov")) {
      for (per in rev(colnames(output)[which(output[sel,] == 1)])) {
        if (per == "Day") {
          moutputs[[paste0(toupper(sel),"DAY.ASC")]] <- X[,lapply(.SD, round, 1), by = list(yr, mo, da, doy)]
        }
        if (per == "Mon") {
          moutputs[[paste0(toupper(sel),"MON.ASC")]] <- X[,lapply(.SD, function(x) {round(sum(x),1)}),
                                                          .SDcols = -c("da","doy"), by = list(yr, mo)]
        }
        if (per == "Ann") {
          moutputs[[paste0(toupper(sel),"ANN.ASC")]] <- X[,lapply(.SD, function(x) {round(sum(x),1)}),
                                                          .SDcols = -c("mo","da","doy"),by = yr]
        }
      }
    }

    if (sel  == "Swat") {
      for (per in rev(colnames(output)[which(output[sel,] == 1)])) {
        if (per == "Day") {
          moutputs[[paste0(toupper(sel),"DAY.ASC")]] <- X[,lapply(.SD, round ,3), by = list(yr, mo, da, doy, nl)]

        }
        if (per == "Mon") {
          moutputs[[paste0(toupper(sel),"MON.ASC")]] <- X[,lapply(.SD, function(x) {round(mean(x),3)}),
                                                          .SDcols = -c("da","doy"), by = list(yr, mo, nl)]
        }
        if (per == "Ann") {
          moutputs[[paste0(toupper(sel),"ANN.ASC")]] <- X[,lapply(.SD, function(x) {round(mean(x),3)}),
                                                          .SDcols = -c("mo","da","doy"),by = list(yr, nl)]
        }
      }
    }
    if (sel  == "Belo") {
      for (per in rev(colnames(output)[which(output[sel,] == 1)])) {
        if (per == "Day") {
          moutputs[[paste0(toupper(sel),"DAY.ASC")]] <- X[,lapply(.SD, round, 1), by = list(yr, mo, da, doy, nl)]

        }
        if (per == "Mon") {
          moutputs[[paste0(toupper(sel),"MON.ASC")]] <- X[,lapply(.SD, function(x) {round(sum(x),1)}),
                                                          .SDcols = -c("da","doy"),by = list(yr, mo, nl)]
        }
        if (per == "Ann") {
          moutputs[[paste0(toupper(sel),"ANN.ASC")]] <- X[,lapply(.SD, function(x) {round(sum(x),1)}),
                                                          .SDcols = -c("mo","da","doy"), by = list(yr, nl)]
        }
      }
    }
    if (sel  == "Budg") {
      for (per in rev(colnames(output)[which(output[sel,] == 1)])) {
        if (per == "Day") {
          moutputs[[paste0(toupper(sel),"DAY.ASC")]] <- X[,list(# for fluxes: sum up over period
                                                                prec = round(sum(rfal+sfal),1),
                                                                flow = round(sum(flow),1),
                                                                evap = round(sum(evap),1),
                                                                seep = round(sum(seep),1),
                                                                # for state variables: take last entry in period
                                                                snow = round(snow[which.max(doy)],1),
                                                                swat = round(swat[which.max(doy)],1),
                                                                gwat = round(gwat[which.max(doy)],1),
                                                                intr = round(intr[which.max(doy)],1),
                                                                ints = round(ints[which.max(doy)],1)),
                                                          by = list(yr, mo, da, doy)]
        }
        if (per == "Mon") {
          moutputs[[paste0(toupper(sel),"MON.ASC")]] <- X[, list(# for fluxes: sum up over period
                                                                 prec = round(sum(rfal+sfal),1),
                                                                 flow = round(sum(flow),1),
                                                                 evap = round(sum(evap),1),
                                                                 seep = round(sum(seep),1),
                                                                 # for state variables: take last entry in period
                                                                 snow = round(snow[which.max(doy)],1),
                                                                 swat = round(swat[which.max(doy)],1),
                                                                 gwat = round(gwat[which.max(doy)],1),
                                                                 intr = round(intr[which.max(doy)],1),
                                                                 ints = round(ints[which.max(doy)],1)),
                                                          by = list(yr, mo)]
        }
        if (per == "Ann") {
          moutputs[[paste0(toupper(sel),"ANN.ASC")]] <- X[, list(# for fluxes: sum up over period
                                                                 prec = round(sum(rfal+sfal),1),
                                                                 flow = round(sum(flow),1),
                                                                 evap = round(sum(evap),1),
                                                                 seep = round(sum(seep),1),
                                                                 # for state variables: take last entry in period
                                                                 snow = round(snow[which.max(doy)],1),
                                                                 swat = round(swat[which.max(doy)],1),
                                                                 gwat = round(gwat[which.max(doy)],1),
                                                                 intr = round(intr[which.max(doy)],1),
                                                                 ints = round(ints[which.max(doy)],1)),
                                                          by = list(yr)]
        }
      }
    }
    if (sel  == "Misc") {
      for (per in rev(colnames(output)[which(output[sel,] == 1)])) {
        if (per == "Day") {
          moutputs[[paste0(toupper(sel),"DAY.ASC")]] <- X[, list(
                                                                 vrfln   = round(vrfln,1),
                                                                 safrac  = round(safrac,1),
                                                                 stres   = round(stres,3),
                                                                 adef    = round(adef,3),
                                                                 awat    = round(awat,1),
                                                                 relawat = round(relawat,3),
                                                                 nits,
                                                                 balerr  = round(balerr, 3)),
                                                          by = list(yr, mo, da, doy)]

        }
        if (per == "Mon") {
          moutputs[[paste0(toupper(sel),"MON.ASC")]] <- X[, list(vrfln   = round(sum(vrfln),1),
                                                                 safrac  = round(sum(safrac),1),
                                                                 stres   = round(mean(stres),3),
                                                                 adef    = round(mean(adef),3),
                                                                 awat    = round(mean(awat),1),
                                                                 relawat = round(mean(relawat),3),
                                                                 nits    = sum(nits),
                                                                 balerr  = round(sum(balerr), 3)),
                                                          by = list(yr, mo)]
        }
        if (per == "Ann") {
          moutputs[[paste0(toupper(sel),"ANN.ASC")]] <- X[, list(vrfln   = round(sum(vrfln),1),
                                                                 safrac  = round(sum(safrac),1),
                                                                 stres   = round(mean(stres),3),
                                                                 adef    = round(mean(adef),3),
                                                                 awat    = round(mean(awat),1),
                                                                 relawat = round(mean(relawat),3),
                                                                 nits    = sum(nits),
                                                                 balerr  = round(sum(balerr), 3)),
                                                          by = list(yr)]
        }
      }
    }
  }


  return(moutputs)
}
