#' Run the LWF-Brook90 hydrological model
#'
#' Sets up the input objects for the LWF-Brook90 hydrological model, starts the model,
#' and returns the selected results.
#'
#' @param project.dir directory-name of the project to which output files
#' are written. Will be created, if not existing. Defaults to 'runLWFB90/'
#' @param options.b90 named list of model control options. Use
#' \code{\link{setoptions_LWFB90}} to generate a list with default model control options.
#' @param param.b90 Named list of model input parameters. Use
#' \code{\link{setparam_LWFB90}} to generate a list with default model parameters.
#' @param climate data.frame with daily climate data. See details for the required variables.
#' @param precip data.frame with columns 'dates' and 'prec' to supply precipitation data separately from climate data.
#' Can be used to provide sub-day resolution precipitation data to LWFBrook90. For each day in dates,
#' 1 (daily resolution) to 240 values of precipitation can be provided, with the number of values
#' per day defined in \code{options.b90$prec.interval}.
#' @param soil data.frame containing the hydraulic properties of the soil layers. See details.
#' @param output a [10,5]-matrix flagging the desired model-output. Use
#' \code{\link{setoutput_LWFB90}} to generate and edit a default output matrix.
#' @param output_fun a function or a list of functions to be performed on the output objects selected by \code{output}.
#' Can be used to aggregate output or calculate goodness of fit measures.
#' Disabled when read.output = FALSE.
#' @param rtrn.input append 'param.b90', 'options.b90', 'soil' and daily plant
#' properties ('standprop_daily', as derived from parameters) to the result?
#' @param rtrn.output return the simulation results? Disabled when read.output = FALSE.
#' @param read.output read the simulation result files from project.dir? Default is TRUE.
#' @param chk.input logical wether to check param.b90, options.b90, climate, precip, soil, and obs
#' for completeness and consistency.
#' @param output.log write the logfile 'Log.txt' to the 'project.dir'? Default is TRUE.
#' @param run run LWF-Brook90 or only return model input objects?
#' Useful to inspect the effects of options and parameters on model input. Default is TRUE.
#' @param verbose print messages to the console? Default is TRUE.
#' @param ... additional arguments passed to \code{output_fun}.
#'
#' @return A list containing the model input (except for climate), the contents of
#' the LWF-Brook90 output files found in 'project.dir', and the return values of \code{output_fun}.
#'
#'@section Climate data:
#' The \code{climate} data.frame must contain the following variable in columns named
#' \code{dates} (Date), \code{tmax} (deg C), \code{tmin} (deg C), \code{tmean} (deg C),
#' \code{wind} (m s-1), \code{prec} (mm) , \code{vappres} (kPa), and either \code{globrad} (MJ d-1 m-2)
#' or \code{sunhours} (hours). When using \code{sunhours}, please set \code{options.b90$fornetrad = 'sunhours'}.
#'
#' @section Soil parameters:
#' Each row of \code{soil} represents one layer, containing the layers' boundaries and soil hydraulic parameters.
#' The column names for the upper and lower layer boundaries are \code{upper} and \code{lower} (m, negative downwards).
#' When using options.b90$imodel = 'MvG', the hydraulic parameters are  \code{ths}, \code{thr},
#'  \code{alpha} (m-1), \code{npar}, \code{ksat} (mm d-1) and \code{tort}.  With options.b90$imodel = 'CH',
#'  the parameters are \code{thsat}, \code{thetaf}, \code{psif}(kPa), \code{bexp},
#'  \code{kf} (mm d-1), and \code{wetinf}. For both parameterizations, the volume fraction of stones has to be named \code{gravel}.
#'  If the soil data.frame is not provided, list items 'soil_nodes' and 'soil_materials'
#'  of param.b90 are used for the simulation. These have to be set up in advance, see \code{\link{soil_to_param}}.
#'
#' @export
#' @examples
#' # Set up lists containing model control options and model parameters:
#' param.b90 <- setparam_LWFB90()
#' options.b90 <- setoptions_LWFB90()
#'
#' # Set start and end Dates for the simulation
#' options.b90$startdate <- as.Date("2002-01-01")
#' options.b90$enddate <- as.Date("2003-12-31")
#'
#' # Derive soil hydraulic properties from soil physical properties
#' # using pedotransfer functions
#' soil <- cbind(slb1_soil, hydpar_wessolek_mvg(slb1_soil$texture))
#'
#' # Run LWF-Brook90
#' b90.result <- runLWFB90(project.dir = "example_run_b90",
#'                         options.b90 = options.b90,
#'                         param.b90 = param.b90,
#'                         climate = slb1_meteo,
#'                         soil = soil)
#'
#' # use a function to be performed on the output:
#' # aggregate soil water storage down to a specific layer
#' agg_swat <- function(x, layer) {
#'   out <- aggregate(SWATI~YR+DOY,
#'                    x$SWATDAY.ASC,
#'                    FUN = sum,
#'                    subset = NL <= layer)
#'   out[order(out$YR, out$DOY),]}
#'
#' # run model, without returning the original output.
#' b90.aggswat <- runLWFB90(project.dir = "example_run_b90",
#'                          options.b90 = options.b90,
#'                          param.b90 = param.b90,
#'                          climate = slb1_meteo,
#'                          soil = soil,
#'                          output_fun = list(swat = agg_swat),
#'                          rtrn.output = FALSE,
#'                          layer = 10) #' passed to output_fun
#' str(b90.aggswat$output_fun$swat)
#'
runLWFB90 <- function(project.dir = "runLWFB90/",
                      options.b90,
                      param.b90,
                      climate,
                      precip = NULL,
                      soil = NULL,
                      output = setoutput_LWFB90(),
                      output_fun = NULL,
                      rtrn.input = TRUE,
                      rtrn.output = TRUE,
                      read.output = TRUE,
                      chk.input = TRUE,
                      output.log = TRUE,
                      run = TRUE,
                      verbose = TRUE,
                      ...) {

  oldWD <- getwd()
  on.exit(setwd(oldWD))

  # input checks ------------------------------------------------------------
  if (chk.input) {
    chk_options()
    chk_param()
    chk_clim()
    chk_soil()
    #chk_obs()
  }

  # ---- Simulation period ----------------------------------------------------------
  climyears <- unique(climate$yr)
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
    param.b90$soil_nodes$rootden <- MakeRelRootDens(soilnodes = param.b90$soil_nodes$lower,
                                                    maxrootdepth = param.b90$maxrootdepth,
                                                    method = options.b90$root.method,
                                                    beta = param.b90$betaroot,
                                                    relrootden = param.b90$rootden.tab$rootden,
                                                    rootdepths = param.b90$rootden.tab$depth)
  } else {
    if (!is.null(soil)) {
      param.b90$soil_nodes$rootden <- soil$rootden
    } else {
      stopifnot(!is.null(param.b90$soil_nodes$rootden))
    }
  }

  # ---- Execute LWF-Brook90  -------------------------------------------------------
  if (run) {
    # create project-directory
    project.dir <- normalizePath(project.dir, mustWork = FALSE)

    if (!dir.exists(project.dir)) {
      if (verbose == TRUE) {message("Creating project-directory...")}
      tryCatch( {
        dir.create(project.dir)
      }, warning = function(wrn){
        stop(paste0("The specified  project directory (",
                    project.dir,
                    ") could not be created.)"))
      },
      error = function(err){
        return(err)
      })
    }

    setwd(project.dir) # set the working directory to the project folder
    try(file.remove(list.files(project.dir, pattern = ".ASC", full.names = T)))
    try(file.remove(list.files("Log.txt")))

    if (verbose == T) {
      message("Running model..." )
    }

    start <- Sys.time()
    r_lwfbrook90(
      siteparam = data.frame(simyears[1],
                             as.integer(format(options.b90$startdate, "%j")),
                             param.b90$coords_y, param.b90$snowini, param.b90$gwatini,
                             options.b90$prec.interval),
      climveg = cbind(climate[, c("yr", "mo", "da","globrad","tmax","tmin",
                                  "vappres","wind","prec","mesfl")],
                      standprop_daily[, c("densef", "height", "lai", "sai", "age")]),
      precdat = precip,
      param = param_to_rbrook90(param.b90, options.b90$imodel),
      pdur = param.b90$pdur,
      soil_materials = param.b90$soil_materials,
      soil_nodes = param.b90$soil_nodes[,c("layer","midpoint", "thick", "mat", "psiini", "rootden")],
      output = output,
      output_log = output.log
    )

    finishing_time <- Sys.time()
    simtime <- finishing_time - start
    units(simtime) <- "secs"

    if (verbose == T) {
      message(paste("Simulation successful! Duration:", round(simtime,2), "seconds"))
    }

    # initialize return ---------------------------------------------------------------
    simres <- list(simulation_duration = simtime,
                   finishing_time = finishing_time)

    # ---- Read output files --------------------------------------------------------
    if ( read.output ) {
      if (verbose == T) {
        message("Reading output...")
      }
      simout <- lapply(list.files(project.dir, pattern = ".ASC", full.names = T),
                       data.table::fread,
                       fill = T, stringsAsFactors = FALSE)
      names(simout) <- list.files(project.dir, pattern = ".ASC")

      # append results
      if (rtrn.output) {
        simres[names(simout)] <- simout
      }


      # ---- apply functions on simuation output -------------------------------
      if (!is.null(output_fun)) {
        if (verbose == T) {
          message("Applying function on simulation output files..")
        }
        if (!is.list(output_fun)){
          output_fun <- list(output_fun)
        }

        simres$output_fun <- tryCatch( {

          lapply(output_fun, do.call, args = list(simout, ...))

        },
        warning = function(wrn){return(wrn)},
        error = function(err){return(err)})
      }
    }

    # ---- append model input -------------------------------------------------------
    if (rtrn.input) {
      simres$model_input <- list(options.b90 = options.b90,
                                 param.b90 = param.b90,
                                 standprop_daily = standprop_daily)
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


chk_options <- function(){
  eval.parent(quote({
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

## check-functions for input

chk_param <- function() {
  eval.parent(quote({
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

  eval.parent(quote({ # manipualte the calling environment

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

chk_obs <- function(){
  eval.parent(quote({
    if (!is.null(obs)) {
      names(obs) <- tolower(names(obs))
      stopifnot("dates" %in% names(obs),
                inherits(obs$dates, "Date"),
                length(obs) > 1)
      if (min(obs$dates) > options.b90$startdate & max(obs$dates) < options.b90$enddate) {
        stop("Your observations are not within the simulation period.")
      }
      if (is.null(gof_fun)) {
        stop("Please provide a function(sim, obs) or list of functions to calculate
           goodness-of-fit measures for observed variables.")
      }
    }
  }))
}

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





