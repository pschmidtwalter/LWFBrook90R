#' Run the LWF-Brook90 hydrological model and return results
#'
#' Sets up the input objects for LWF-Brook90 hydrological model, starts the model,
#' and returns the results.
#' @param project.dir directory-name of the project to which output files
#' are written. Will be created, if not existing.
#' @param options.b90 named list of model control options. Use
#' \code{\link{setoptions_LWFB90}} to generate a list with default model control options.
#' @param param.b90 Named list of model input parameters. Use
#' \code{\link{setparam_LWFB90}} to generate a list with default model parameters.
#' @param climate data.frame with daily climate data. The names of climate have to
#' correspond to arguments \emph{dates}, \emph{tmax}, \emph{tmin}, \emph{wind}, \emph{prec}, \emph{vappres},
#' \emph{globrad}, \emph{sunhours}) of \code{\link{writeClimate.in}}.
#' @param precip data.frame to supply precipitation data separately from climate data.
#' Can be used to provide sub-day resolution precipitation data to LWFBrook90.
#' @param soil data.frame containing the hydraulic properties of the soil layers.
#' Each row represents one layer, containing the layers' boundaries and soil hydraulic parameters.
#' The columns names for the upper and lower layer boundaries are \emph{upper} and \emph{lower} (m, negative downwards),
#' the parameters of the van Genuchten retention functions are \emph{ths}, \emph{thr},
#'  \emph{alpha} [m-1], \emph{npar}, and the parameters of the Mualem conductivity function
#'  \emph{ksat} [mm d-1] and \emph{tort}. The volume fraction of stones has to be named \emph{gravel}.
#'  If the soil data.frame is not provided, list items soil_nodes and soil_materials of param.b90 are used for the simulation.
#'  These have to be set up in advance.
#' @param outputmat a [10,5]-matrix flagging the desired model-output. Use
#' \code{\link{setoutput_LWFB90}} to generate and edit a default output matrix.
#' @param output.param.options append 'param.b90', 'options.b90', 'soil' and daily plant
#' properties ('plant.devt', as derived from parameters and written to 'climate.in') to the result?
#' @param read.output read and return simulation results from out.dir? Default is TRUE.
#' @param output.log write the logfile Log.txt? Default is TRUE.
#' @param verbose print messages to the console? Default is TRUE.
#' @param run run LWF-Brook90 or only return model input objects?
#' Useful to inspect the effects of options and parameters on model input. Default is TRUE.
#'
#' @return Returns the model-output from the files found in 'project.dir' as a list of data.tables,
#' along with the execution time of the simulation, and model input if desired.
#' @export
#' @import vegperiod
#' @examples
#'
#' #Set up lists containing model control options and model parameters:
#'
#' param.b90 <- setparam_LWFB90()
#' options.b90 <- setoptions_LWFB90()
#'
#' # Set start and end Dates for the simulation
#'
#' options.b90$startdate <- as.Date("2000-01-01")
#' options.b90$enddate <- as.Date("2004-12-31")
#'
#' # Derive soil hydraulic properties from soil physical properties
#' # using pedotransfer functions
#'
#' soil <- cbind(soil_slb1, hydpar_wessolek_mvg(soil_slb1$texture))
#'
#' # Run LWF-Brook90
#' b90.result <- runLWFB90(project.dir = "example_run_b90",
#'                       options.b90 = options.b90,
#'                       param.b90 = param.b90.b90,
#'                       climate = meteo_slb1,
#'                       soil = soil)
runLWFB90 <- function(project.dir,
                      options.b90,
                      param.b90,
                      climate,
                      precip = NULL,
                      soil = NULL,
                      outputmat = setoutput_LWFB90(),
                      output.param.options = TRUE,
                      run = TRUE,
                      read.output = TRUE,
                      output.log = TRUE,
                      verbose = TRUE
){

  oldWD <- getwd()
  on.exit(setwd(oldWD))

  #name-checks ----------------------------------------------------------------------
  names(options.b90) <- tolower(names(options.b90))
  names(param.b90) <- tolower(names(param.b90))
  names(climate) <- tolower(names(climate))

  options.b90$fornetrad <- match.arg(options.b90$fornetrad, choices = c("globrad","sunhour"))

  options.b90$standprop.input <- match.arg(options.b90$standprop.input, choices = c("parameters", "table"))

  options.b90$lai.method <- match.arg(options.b90$lai.method, choices = c("b90", "linear", "Coupmodel"))

  options.b90$root.method <- match.arg(options.b90$root.method, choices = c("betamodel", "table", "linear", "constant", "soilvar"))
  options.b90$imodel <- match.arg(options.b90$imodel, choices = c("MvG", "CH"))

  if (options.b90$budburst.method %in% c("const", "const.", "constant")) options.b90$budburst.method <- "fixed"
  if (options.b90$leaffall.method %in% c("const", "const.", "constant")) options.b90$leaffall.method <- "fixed"

  # climate name checks
  stopifnot(all(c("dates", "tmax", "tmin",options.b90$fornetrad, "vappres", "wind") %in% names(climate)))

  if (is.null(precip) ){
    stopifnot("prec" %in% names(climate))
  } else {
    names(precip) <- tolower(names(precip))
    stopifnot(all(c("dates","prec") %in% names(precip)))
  }

  # soil names
  if (is.null(soil)) {
    stopifnot(all(c( "upper", "lower", "mat") %in% names(param.b90$soil_nodes)))
    if (options.b90$imodel == "MvG" ) {
      stopifnot(all(c("mat","ths","thr","alpha","npar","ksat","tort","gravel") %in% names(param.b90$soil_materials)))
    } else {
      stopifnot(all(c("mat","thsat","thetaf","psif","bexp","kf","wetinf","gravel") %in% names(param.b90$soil_materials)))
    }
  } else {
    if (options.b90$imodel == "MvG") {
      stopifnot(all(c("upper","lower", "ths","thr","alpha","npar","ksat","tort","gravel") %in% names(soil)))
    } else {
      stopifnot(all(c("upper","lower", "thsat","thetaf","psif","bexp","kf","wetinf","gravel") %in% names(soil)))
    }
  }


  # ---- Input checks ---------------------------------------------------------------

  if (!inherits(options.b90$startdate, "Date")) {
    stop("Invalid argument: 'options.b90$startdate'")}
  if (!inherits(options.b90$enddate, "Date")) {
    stop("Invalid argument: 'options.b90$enddate'")}
  if (!(options.b90$startdate < options.b90$enddate)) {
    stop("Invalid arguments: 'startdate > enddate ")}


  if ( is.null(soil) & (is.null(param.b90$soil_nodes) || is.null(param.b90$soil_materials))) {
    stop("Please provide soil data, either via the argument 'soil' or as list items 'soil_nodes' and 'soil_materials' in param.b90 ")
  }

  if (options.b90$root.method == "soilvar") {
    if (is.null(soil)) {
      stop("Please provide the 'soil'-argument when using options.b90$root.method = 'soilvar'.")
    } else {
      if (is.null(soil$rootden)) {
        stop("Please provide the column 'rootden' in 'soil'-data.frame")
      }
    }
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

  if (!any( names(climate) == "mesfl") ) {
    climate$mesfl <- 0
  }
  if (!is.null(precip)){
    if (!any( names(precip) == "mesfl") ) {
      precip$mesfl <- 0
    }
  }

  # ---- clean file paths and set up directories -----------------------------------------

  # create project-directory
  if (run) {
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
  }

  # ---- Simulation period ----------------------------------------------------------
  climyears <- unique(year(climate$dates))
  simyears <- seq(from = as.integer(format(options.b90$startdate,"%Y")),
                  to = as.integer(format(options.b90$enddate,"%Y")),
                  by = 1)

  if (length(simyears[which(simyears %in% climyears)]) < length(simyears)) {
    if (length(simyears[which(simyears %in% climyears)]) == 0) {
      stop("Your climate data does not include the simulation period. Change startdate and enddate!")
    } else {
      warning("climate not covering simulation period completely, period was cut!")
    }
  }

  # Number of Simulation days
  param.b90$ndays <-  as.integer(difftime(options.b90$enddate,options.b90$startdate)) + 1


  # Vegetation-Period: calculate budburst and leaffall-doys  ------------------------
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

  # ---- Make Stand --------------------------------------------------------------------
  # Prepare parameters
  if (tolower(options.b90$standprop.input) == "table") {
    if (verbose == T) {message("Creating long term stand dynamics from table 'standprop.table'...")}
    param.b90 <- standprop_yearly_to_param(param.b90$standprop_yearly,
                                           param.b90,
                                           out.years = simyears)
  } else {
    if (verbose == T) {message("Creating constant stand properties from parameters...")}
    # derive age from age.ini for simyears
    param.b90$age <- seq(from = param.b90$age.ini + 1,
                         by = 1, length.out = length(simyears))

  }

  # ---- Create standproperties from parameters

  standprop_daily <- make_standprop(options.b90, param.b90, out.years = simyears )

  # constrain to simulation period
  standprop_daily <- standprop_daily[which(dates >= options.b90$startdate
                                           & dates <= options.b90$enddate),]

  if (verbose == T) {
    message("Standproperties created succesfully")
  }

  # ---- Prepare climate for input----------------------------------------------------

  # Precipitation correction (Richter)
  if (options.b90$prec.corr == TRUE) {
    climate[, prec := prec_corr(dates = dates, tavg = tmean, prec = prec,
                                station.exposure = options.b90$exposure.prec)]
  }

  #Calculate global radiation from sunshine duration
  if (options.b90$fornetrad == "sunhour") {
    climate[,globrad := CalcGlobRad( yday(dates), sunhours, param.b90$coords_y )]
  }

  # constrain data to simulation period
  climate <- climate[which(dates >= options.b90$startdate & dates <= options.b90$enddate),]

  if (!is.null(precip)) {
    precip <- precip[which(dates >= options.b90$startdate & dates <= options.b90$enddate),]
    if (nrow(climate)*options.b90$prec.interval != nrow(precip)) {
      warning("The precipitation data provided via precip does not fit to the precipitation interval defined in options.b90$prec.interval.
              Precip-argument will be ignored and prec from climate data will be used.")
      options.b90$prec.interval <- 1
      precip <- NULL
      stopifnot("prec" %in% names(climate))
    } else {
      if (options.b90$prec.interval == 1) {
        climate$prec <- precip$prec
        precip <- NULL
      } else {
        precip[,ii := rep(1:options.b90$prec.interval,param.b90$ndays)]
        precip <- precip[, list(year(dates), month(dates), mday(dates), ii, prec, mesfl)]
        climate$prec <- -999
      }
    }
  }
  # ---- Make soilnodes & soil materials --------------------------------------------
  # soil provided as argument -> create soil nodes and materials and add them to param.b90
  if (!is.null(soil)) {
    soil_nodes_mat <- soil_to_param(soil, options.b90$imodel)
    param.b90$soil_nodes <- soil_nodes_mat$soil_nodes
    param.b90$soil_materials <- soil_nodes_mat$soil_materials
  }

  if (options.b90$imodel == "MvG") {
    param.b90$soil_materials <- param.b90$soil_materials[,list(mat,ths,thr,alpha,npar,ksat,tort,gravel)]
  } else {
    param.b90$soil_materials <- param.b90$soil_materials[,list(mat,thsat,thetaf,psif,bexp,kf,wetinf,gravel)]
  }

  param.b90$soil_nodes$thick <- param.b90$soil_nodes$upper - param.b90$soil_nodes$lower
  param.b90$soil_nodes$midpoint <- param.b90$soil_nodes$lower + param.b90$soil_nodes$thick/2
  param.b90$soil_nodes$thick <- round(param.b90$soil_nodes$thick * 1000) # mm
  param.b90$soil_nodes$layer <- 1:nrow(param.b90$soil_nodes)
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
    param.b90$soil_nodes$rootden <- soil$rootden
  }
  if (!is.data.table(param.b90$soil_nodes)) {
    setDT(param.b90$soil_nodes)
  }

  if (!is.data.table(param.b90$soil_materials)) {
    setDT(param.b90$soil_materials)
  }
  # ---- Execute LWF-Brook90  -------------------------------------------------------
  if (run) {
    if (verbose == T) {
      message("Running model..." )
    }

    start <- Sys.time()
    r_lwfbrook90(
      siteparam = data.frame(simyears[1],
                             as.integer(format(options.b90$startdate, "%j")),
                             param.b90$coords_y, param.b90$snowini, param.b90$gwatini,
                             options.b90$prec.interval),
      climveg = climate[, list(year(dates), month(dates), mday(dates),
                               globrad,
                               tmax,
                               tmin,
                               vappres,
                               wind,
                               prec,
                               mesfl,
                               standprop_daily$densef,
                               standprop_daily$height,
                               standprop_daily$lai,
                               standprop_daily$sai,
                               standprop_daily$age)],
      precdat = precip,
      param = param_to_rbrook90(param.b90, options.b90$imodel),
      pdur = param.b90$pdur,
      soil_materials = param.b90$soil_materials,
      soil_nodes = param.b90$soil_nodes[,list(layer,midpoint, thick, mat, psiini, rootden)],
      output = outputmat,
      output_log = output.log
    )

    simtime <- Sys.time() - start
    units(simtime) <- "secs"

    if (verbose == T) {
      message(paste("Simulation successful! Duration:", round(simtime,2), "seconds"))
      message("Reading output...")
    }

    # ---- Read output files ----------------------------------------------------------
    if ( read.output ) {
      simres <- lapply(list.files(project.dir, pattern = ".ASC", full.names = T), fread, fill = T, stringsAsFactors = F)
      names(simres) <- list.files(project.dir, pattern = ".ASC")
    } else {
      simres <- list()
    }

  } else { #'dry' run
    simres <- list(options.b90 = options.b90,
                   param.b90 = param.b90,
                   plant.devt = data.table(standprop_daily))
    return(simres)
  }

  # append model input
  if (output.param.options == TRUE){
    simres$model_input <- list(options.b90 = options.b90,
                               param.b90 = param.b90,
                               plant.devt = data.table(standprop_daily))
  }

  if (verbose == T) {
    message("Finished!")
  }
  simres$finishing_time <- Sys.time()
  simres$sim_duration <- simtime
  return(simres)
}

