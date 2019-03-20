#' Run the LWF-Brook90 hydrological model and return results
#'
#' Takes all necessary information needed to run the LWF-Brook90 hydrological model,
#' writes input files, starts the external executable via a system-call and returns
#' the results.
#' #' @param project.dir directory-name of the project where the input and output files
#' are located. Will be created, if not existing.
#' @param options.b90 named list of model control options. Use
#' \code{\link{MakeOptions.B90}} to generate a list with default model control options.
#' @param param.b90 Named list of model input parameters. Use
#' \code{\link{MakeParam.B90}} to generate a list with default model parameters.
#' @param climate data.frame with daily climate data. The names of climate have to
#' correspond to arguments \emph{dates}, \emph{tmax}, \emph{tmin}, \emph{wind}, \emph{prec}, \emph{vappres},
#' \emph{globrad}, \emph{sunhours}) of \code{\link{writeClimate.in}}.
#' @param soil data.frame containing the hydraulic properties of the soil layers.
#' Each row represents one layer, containing the layers' boundaries and soil hydraulic parameters.
#' The columns names for the upper and lower layer boundaries are \emph{upper} and \emph{lower} (m, negative downwards),
#' the parameters of the van Genuchten retention functions are \emph{ths}, \emph{thr},
#'  \emph{alpha} [m-1], \emph{npar}, and the parameters of the Mualem conductivity function
#'  \emph{ksat} [mm d-1] and \emph{tort}. The volume fraction of stones has to be named \emph{gravel}.
#'  If the soil data.frame is not provided, list items soil_nodes and soil_materials of param.b90 are used for the simulation.
#'  These have to be set up in advance.
#' @param outputmat a [10,5]-matrix flagging the desired model-output. Use
#' \code{\link{choose_output.B90}} to generate and edit default output matrix.
#' @param out.dir path where to write the output-files.
#' @param output.param.options append 'param.b90', 'options.b90', 'soil' and daily plant
#' properties ('plant.devt', as derived from parameters and written to 'climate.in') to the result?
#' @param keep.log.on.success keep the file 'output.log' after a successful simulation?
#' In case of simulation errors the 'output.log' file (if specified) is kept anyway for inspection purposes.
#' @param keep.outputfiles keep the model .asc output files after running and
#' returning the output?
#' @param verbose print messages to the console? Default is TRUE.
#' @param run.model run the model or only return model input objects? Default is TRUE.
#'
#' @return Returns the model-output from the files found in 'out.dir' as a list of data.tables,
#' along with the execution time of the simulation, and input-parameters and options if desired.
#' @export
#' @import data.table vegperiod
#' @examples
#'
#' #Set up lists containing model control options and model parameters:
#'
#' param.b90 <- MakeParam.B90()
#' options.b90 <- MakeOptions.B90()
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
#' b90.result <- Run.B90(project.dir = "example_run_b90",
#'                       options.b90 = options.b90,
#'                       param.b90 = param.b90.b90,
#'                       climate = meteo_slb1,
#'                       soil = soil,
#'                       path_b90.exe = "b90.exe")


runLWFB90 <- function(project.dir,
                    options.b90,
                    param.b90,
                    climate,
                    soil = NULL,
                    outputmat = setoutput_LWFB90(),
                    keep.log.on.success = T,
                    out.dir = "out/",
                    output.param.options = TRUE,
                    keep.outputfiles = TRUE,
                    verbose = TRUE
){

  oldWD <- getwd()
  on.exit(setwd(oldWD))

  #name-checks ----------------------------------------------------------------------
  names(options.b90) <- tolower(names(options.b90))
  names(param.b90) <- tolower(names(param.b90))
  names(climate) <- tolower(names(climate))

  options.b90$fornetrad <- match.arg(options.b90$fornetrad, choices = c("globrad","sunhour"))

  options.b90$budburst.method <- match.arg(options.b90$budburst.method,
                                           choices = c("fixed", "constant","Menzel","StdMeteo", "ETCCDI", "Ribes uva-crispa"))

  options.b90$leaffall.method <- match.arg(options.b90$leaffall.method,
                                           choices = c("fixed", "constant","vonWilpert", "LWF-BROOK90", "NuskeAlbert", "StdMeteo","ETCCDI"))

  options.b90$standprop.input <- match.arg(options.b90$standprop.input, choices = c("parameters", "table"))

  options.b90$lai.method <- match.arg(options.b90$lai.method, choices = c("b90", "linear", "Coupmodel"))

  options.b90$root.method <- match.arg(options.b90$root.method, choices = c("betamodel", "table", "linear", "constant", "soilvar"))

  # Check suggested packages --------------------------------------------------------
  if (!options.b90$budburst.method %in% c("constant", "fixed") || !options.b90$leaffall.method %in% c("constant", "fixed")) {
    if (!requireNamespace("vegperiod", quietly = TRUE)) {
      stop("In 'options.b90' you chose dynamic budburst or leaf fall, for which the
           package \"vegperiod\" is required. Please install it:
           install.packages('vegperiod')")
    }
    }

  # ---- Input checks ---------------------------------------------------------------

  if (!inherits(options.b90$startdate, "Date")) {
    stop("Invalid argument: 'options.b90$startdate'")}
  if (!inherits(options.b90$enddate, "Date")) {
    stop("Invalid argument: 'options.b90$enddate'")}
  if (!(options.b90$startdate < options.b90$enddate)) {
    stop("Invalid arguments: 'startdate > enddate ")}

  if ( is.null(soil) & (is.null(param.b90$soil_nodes) || is.null(param.b90$soil_material))){
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
  # # Climate
  # if (options.b90$fornetrad == "globrad" & !any( names(climate) == "globrad" )) {
  #   if (any( names(climate) == "sunhour" )) {
  #     options.b90$fornetrad <- "sunhour"
  #     warning("Global radiation missing, will be calculated from sunshine duration!")
  #   } else {
  #     stop("Please either provide globarad or sunhours with your climate file!")
  #   }
  # } else {
  #   if (any( names(climate) == "globrad" )) {
  #     options.b90$fornetrad <- "globrad"
  #     stop("Please either provide 'globrad' or 'sunhours' with your climate!")}
  # }
  #
  if (!any( names(climate) == "mesfl") ){
    climate$mesfl <- 0
  }

  # ---- clean file paths and set up directories -----------------------------------------

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

  # output-directory: variable! But, in Param.in needs to be shorter than 80 characters!
  options.b90$out.dir <- normalizePath(out.dir, mustWork = FALSE)

  # if normalized output-name too long, and not inside 'project.dir' make out.dir within 'project.dir':
  if (nchar(options.b90$out.dir) > 80 &
      options.b90$out.dir != normalizePath(file.path(getwd(), basename(options.b90$out.dir)), mustWork = F) ) {
    warning(paste0("The specified output directory (",options.b90$out.dir,") is too long
                   and could not be read by the model. Find the results in ",basename(options.b90$out.dir),
                   " within the project directory instead!"))
    options.b90$out.dir <- basename(options.b90$out.dir)
  }

  #Create output directory:
  if (!dir.exists(options.b90$out.dir) ) {
    tryCatch( {
      dir.create(options.b90$out.dir)
    }, warning = function(wrn){
      options.b90$out.dir <- basename(options.b90$out.dir)
      dir.create(options.b90$out.dir)
      warning(paste0("The specified output directory (",options.b90$out.dir,") could
                     not be created. Find the results in ",basename(options.b90$out.dir),
                     " within the project directory instead!"))
    })
  }

  #clear output and log
  try(file.remove(list.files(options.b90$out.dir, pattern = ".csv", full.names = T)))


  # ---- Simulation period ----------------------------------------------------------------
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


  # ---- Vegetation-Period  ----------------------------------------------------------
  # check length of fixed leaffall
  if (options.b90$leaffall.method %in% c("constant", "fixed")) {
    if (length(param.b90$leaffalldoy) > 1 & length(param.b90$leaffalldoy) != length(simyears)) {
      stop("When options.b90$leaffall.method == 'fixed', either provide a single value
           for param.b90$leaffall or a sequence of values, one for each year of the simulation period.")
    }
    }
  #check length of fixed budburst
  if (options.b90$budburst.method %in% c("constant", "fixed") ) {
    if (length(param.b90$budburstdoy) > 1 & length(param.b90$budburstdoy) != length(simyears)) {
      stop("When options.b90$budburst.method == 'fixed', either provide a single value
           for param.b90$budburstdoy or a sequence of values, one for each year of the simulation period.")
    }
    }

  # Extend budburst
  if (length(param.b90$budburstdoy) == 1) {
    param.b90$budburstdoy <- rep(param.b90$budburstdoy,times = length(simyears))
  }
  if (length(param.b90$leaffalldoy) == 1) {
    param.b90$leaffalldoy <- rep(param.b90$leaffalldoy,times = length(simyears))
  }

  # start and end dynamic
  if (!options.b90$budburst.method %in% c("constant", "fixed") &
      !options.b90$leaffall.method %in% c("constant", "fixed")) {
    budburst_leaffall <- with(climate, vegperiod::vegperiod(dates = dates,
                                                            Tavg = tmean,
                                                            start.method = as.character(options.b90$budburst.method),
                                                            species = as.character(param.b90$budburst.species),
                                                            end.method = as.character(options.b90$leaffall.method),
                                                            est.prev = ifelse(length(climyears) <= 5, length(climyears) - 1, 5))
    )
    budburst_leaffall <- budburst_leaffall[which(budburst_leaffall$year %in% simyears),]
    param.b90$budburstdoy <- budburst_leaffall$start
    param.b90$leaffalldoy <- budburst_leaffall$end
  } else {
    # only budburst is dynamic:
    if (!options.b90$budburst.method %in% c("constant", "fixed") & options.b90$leaffall.method %in% c("constant", "fixed"))   {
      budburst_leaffall <- with(climate,
                                vegperiod::vegperiod(dates = dates,
                                                     Tavg = tmean,
                                                     start.method = as.character(options.b90$budburst.method),
                                                     species = as.character(param.b90$budburst.species),
                                                     end.method = "StdMeteo",
                                                     est.prev = ifelse(length(climyears) <= 5, length(climyears) - 1, 5))
      )
      param.b90$budburstdoy <- budburst_leaffall$start[which(budburst_leaffall$year %in% simyears)]
    } else {
      # only end dynamic
      if (options.b90$budburst.method %in% c("constant", "fixed") & !options.b90$leaffall.method %in% c("constant", "fixed"))   {
        budburst_leaffall <- with(climate,
                                  vegperiod::vegperiod(dates = dates,
                                                       Tavg = tmean,
                                                       start.method = "StdMeteo",
                                                       end.method = options.b90$leaffall.method))
        options.b90$leaffalldoy <- budburst_leaffall$end[which(budburst_leaffall$year %in% simyears)]
      }
    }
  }


  # ---- Make Stand --------------------------------------------------------------------
  if (tolower(options.b90$standprop.input) == "table") {
    if (verbose == T) {message("Creating long term stand dynamics from table 'standprop.table'...")}

    if (is.null(param.b90$standprop.table) & tolower(options.b90$standprop.input) == "table") {
      stop("param.b90$standprop.table is missing. Required if options.b90$standprop.input = 'table'!")
    }

    if (!any(simyears %in% param.b90$standprop.table$year)) {
      stop("Simulation does not cover any of the years in param.b90$standprop.table")
    }

    # transfer table to parameters
    param.b90$height <- approx(x = as.Date(paste0(param.b90$standprop.table$year,"-12-31")),
                               y = param.b90$standprop.table$height,
                               xout = as.Date(c(paste0(simyears[1],"-01-01"),paste0(simyears,"-12-31"))),
                               method = 'constant', rule = 2)$y
    param.b90$height.ini <- param.b90$height[1]
    param.b90$height <- param.b90$height[-1]

    param.b90$sai <- approx(x = as.Date(paste0(param.b90$standprop.table$year,"-12-31")),
                            y = param.b90$standprop.table$sai,
                            xout = as.Date(c(paste0(simyears[1],"-01-01"),paste0(simyears,"-12-31"))),
                            method = 'constant', rule = 2)$y
    param.b90$sai.ini <- param.b90$sai[1]
    param.b90$sai <- param.b90$sai[-1]

    param.b90$densef <- approx(x = as.Date(paste0(param.b90$standprop.table$year,"-12-31")),
                               y = param.b90$standprop.table$densef,
                               xout = as.Date(c(paste0(simyears[1],"-01-01"),paste0(simyears,"-12-31"))),
                               method = 'constant', rule = 2)$y

    param.b90$densef.ini <- param.b90$densef[1]
    param.b90$densef <- param.b90$densef[-1]

    param.b90$maxlai <- approx(x = as.Date(paste0(param.b90$standprop.table$year,"-01-01")),
                               y = param.b90$standprop.table$maxlai,
                               xout = as.Date(paste0(simyears,"-01-01")),
                               method = 'constant', rule = 2)$y

    #extend or constrain age from table for simyears
    param.b90$age <- seq(param.b90$standprop.table$age[1] - (param.b90$standprop.table$year[1] - min(simyears)),
                         by = 1, length.out = length(simyears))
    #recalculate age.ini
    param.b90$age.ini <- param.b90$age[1] - 1

  } else { if (verbose == T) {message("Creating constant stand properties from parameters...")}

    # derive age from age.ini for simyears
    param.b90$age <- seq(from = param.b90$age.ini+1,
                         by = 1, length.out = length(simyears))

  }

  # interpolate yearly values to daily values
  standprop_daily <- data.table(
    dates = seq.Date(from = as.Date(paste0(min(simyears),"-01-01")),
                     to = as.Date(paste0(max(simyears),"-12-31")),
                     by = "day"),
    age = approx_standprop(x.years = simyears,
                           y = param.b90$age,
                           y.ini = param.b90$age.ini,
                           use_growthperiod = options.b90$standprop.use_growthperiod,
                           startdoy = param.b90$budburstdoy,
                           enddoy = param.b90$leaffalldoy,
                           approx.method = "linear"),
    height = approx_standprop(x.years = simyears,
                              y = param.b90$height,
                              y.ini = param.b90$height.ini,
                              use_growthperiod = options.b90$standprop.use_growthperiod,
                              startdoy = param.b90$budburstdoy,
                              enddoy = param.b90$leaffalldoy,
                              approx.method = options.b90$standprop.interp),
    sai = approx_standprop(x.years = simyears,
                           y = param.b90$sai,
                           y.ini = param.b90$sai.ini,
                           use_growthperiod = options.b90$standprop.use_growthperiod,
                           startdoy = param.b90$budburstdoy,
                           enddoy = param.b90$leaffalldoy,
                           approx.method = options.b90$standprop.interp),
    densef = approx_standprop(x.years = simyears,
                              y = param.b90$densef,
                              y.ini = param.b90$densef.ini,
                              use_growthperiod = options.b90$standprop.use_growthperiod,
                              startdoy = param.b90$budburstdoy,
                              enddoy = param.b90$leaffalldoy,
                              approx.method = options.b90$standprop.interp)
  )

  # daily leaf area index from parameters
  standprop_daily[, lai := MakeSeasLAI(simyears,
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
                                       lai.frac = param.b90$lai.frac)]

  # constrain to simulation period
  standprop_daily <- standprop_daily[which(dates >= options.b90$startdate
                                           & dates <= options.b90$enddate),]

  if (verbose == T) {
    message("Standproperties created succesfully")
  }

  # ---- Prepare climate for input----------------------------------------------------

  # Precipitation corrrection (Richter)
  if (options.b90$richter.prec.corr==TRUE){
    warning("Richter correction of precipitation is not impemented yet. No correction is done!")
    #clim$Precip <- with(clim, RichterKorrPrec(dates=datum,tavg=tmean,precip=prec,exp=b90ini$richterexposition) )
  }

  if (options.b90$fornetrad == "sunhour") {
    climate[,globrad := CalcGlobRad( yday(dates), sunhours, param.b90$coords_y )]
  }

  #constrain to simulation period
  climate <- climate[which(dates >= options.b90$startdate & dates <= options.b90$enddate),]



  # ---- Make soilnodes & soil materials --------------------------------------------
  # soil provided as argument -> create soil nodes and materials and add them to param.b90
  if (!is.null(soil)) {
    soil_nodes_mat <- soil_to_param(soil)
    param.b90$soil_nodes <- soil_nodes_mat$soil_nodes
    param.b90$soil_materials <- soil_nodes_mat$soil_materials
  }

  param.b90$soil_nodes$thick <- param.b90$soil_nodes$upper - param.b90$soil_nodes$lower
  param.b90$soil_nodes$midpoint <- param.b90$soil_nodes$lower + param.b90$soil_nodes$thick/2
  param.b90$soil_nodes$thick <- param.b90$soil_nodes$thick * 1000 # mm
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

  # ---- Execute LWF-Brook90  -------------------------------------------------------
  if (verbose == T) {
    message("Running model..." )
  }

  start <- Sys.time()
  r_brook90(
    site = data.frame(simyears[1],
                      as.integer(format(options.b90$startdate, "%j")),
                      param.b90$coords_y, param.b90$snowini, param.b90$gwatini,
                      options.b90$prec.interval),

    climate = climate[, list(year(dates), month(dates), mday(dates),
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

    param = param_to_rbrook90(param.b90, options.b90$imodel),
    paramYear = param.b90$pdur,
    materials = param.b90$soil_materials,
    soil = param.b90$soil_nodes[,list(layer,midpoint, thick, mat, psiini, rootden)],
    output = outputmat
  )

  simtime <- Sys.time() - start
  units(simtime) <- "secs"

  if (verbose == T) {
    message(paste("Simulation successful! Duration:", round(simtime,2), "seconds"))
    message("Reading output...")
  }

  # ---- Read output files ----------------------------------------------------------
  simres <- lapply(list.files(out.dir, pattern = ".csv", full.names = T), fread, fill = T,stringsAsFactors = F)
  names(simres) <- list.files(out.dir, pattern = ".csv")

  # append input parameters
  if (output.param.options == TRUE) {
    simres$plant.devt <- data.table(standprop_daily)
    simres$param.b90 <- param.b90
    simres$options.b90 <- options.b90
    simres$soil <- soil
  }

  #remove output
  if (!keep.outputfiles) { try(file.remove(list.files(options.b90$out.dir, pattern = ".csv", full.names = T))) }
  if (!keep.log.on.success) { try(file.remove("b90.log"))}

  if (verbose == T) {
    message("Finished!")
  }
  simres$finishing.time <- Sys.time()
  simres$sim.time <- simtime
  return(simres)
  }

