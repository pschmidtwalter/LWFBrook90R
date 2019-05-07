#' Run the LWF-Brook90 hydrological model
#'
#' Sets up the input objects for the LWF-Brook90 hydrological model, starts the model,
#' and returns the results if desired.
#'
#' @param project.dir directory-name of the project to which output files
#' are written. Will be created, if not existing. Defaults to 'runLWFB90/'
#' @param options.b90 named list of model control options. Use
#' \code{\link{setoptions_LWFB90}} to generate a list with default model control options.
#' @param param.b90 Named list of model input parameters. Use
#' \code{\link{setparam_LWFB90}} to generate a list with default model parameters.
#' @param climate data.frame with daily climate data with columns named according to
#'  \emph{dates}, \emph{tmax}, \emph{tmin}, \emph{tmean}, \emph{wind}, \emph{prec}, \emph{vappres},
#' and either \emph{globrad} or \emph{sunhours}).
#' @param precip data.frame with columns 'dates' and 'prec' to supply precipitation data separately from climate data.
#' Can be used to provide sub-day resolution precipitation data to LWFBrook90. For each day in dates,
#' 1 (daily resolution) to 240 values of precipitation can be provided, with the number of values per day defined in options.b90$prec.interval.
#' @param soil data.frame containing the hydraulic properties of the soil layers.
#' Each row represents one layer, containing the layers' boundaries and soil hydraulic parameters.
#' The column names for the upper and lower layer boundaries are \emph{upper} and \emph{lower} (m, negative downwards).
#' When using options.b90$imodel = 'MvG', the hydraulic parameters are  \emph{ths}, \emph{thr},
#'  \emph{alpha} [m-1], \emph{npar}, \emph{ksat} [mm d-1] and \emph{tort}.  With options.b90$imodel = 'CH',
#'  the parameters are \emph{thsat} , \emph{thetaf},\emph{psif} [kPa], \emph{bexp},
#'  \emph{kf} (mm d-1), and \emph{wetinf}. For both parameterizations, the volume fraction of stones has to be named \emph{gravel}.
#'  If the soil data.frame is not provided, list items 'soil_nodes' and 'soil_materials' of param.b90 are used for the simulation.
#'  These have to be set up in advance, see \code{\link{soil_to_param}}.
#' @param output a [10,5]-matrix flagging the desired model-output. Use
#' \code{\link{setoutput_LWFB90}} to generate and edit a default output matrix.
#' @param rtrn.input append 'param.b90', 'options.b90', 'soil' and daily plant
#' properties ('standprop_daily', as derived from parameters) to the result?
#' @param read.output read and return simulation results from project.dir? When FALSE,
#' simulation duration and finishing time are returned. Default is TRUE.
#' @param output.log write the logfile 'Log.txt' to the 'project.dir'? Default is TRUE.
#' @param verbose print messages to the console? Default is TRUE.
#' @param run run LWF-Brook90 or only return model input objects?
#' Useful to inspect the effects of options and parameters on model input. Default is TRUE.
#'
#' @return Returns the model-output from the files found in 'project.dir' as a list of data.tables,
#' along with the execution time of the simulation, and model input if desired.
#' @export
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
runLWFB90 <- function(project.dir = "runLWFB90/",
                      options.b90,
                      param.b90,
                      climate,
                      precip = NULL,
                      soil = NULL,
                      output = setoutput_LWFB90(),
                      rtrn.input = TRUE,
                      read.output = TRUE,
                      output.log = TRUE,
                      chk.input = TRUE,
                      verbose = TRUE,
                      run = TRUE
){

  oldWD <- getwd()
  on.exit(setwd(oldWD))

  # input checks ------------------------------------------------------------
  if (chk.input) {
    chk_options()
    chk_param()
    chk_clim()
    chk_soil()
  }

  # ---- Simulation period ----------------------------------------------------------
  climyears <- unique(year(climate$dates))
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
  standprop_daily <- standprop_daily[which(dates >= options.b90$startdate
                                           & dates <= options.b90$enddate),]

  if (verbose == T) {
    message("Standproperties created succesfully")
  }

  # ---- Prepare climate for input----------------------------------------------------

  # constrain data to simulation period
  climate <- climate[which(dates >= options.b90$startdate & dates <= options.b90$enddate),]

  # Precipitation correction (Richter)
  if (options.b90$prec.corr == TRUE) {
    climate[, prec := prec_corr(dates = dates, tavg = tmean, prec = prec,
                                station.exposure = param.b90$prec.corr.statexp)]
  }

  #Calculate global radiation from sunshine duration
  if (options.b90$fornetrad == "sunhours") {
    climate[,globrad := CalcGlobRad( yday(dates), sunhours, param.b90$coords_y )]
  }

  # ---- Make soilnodes & soil materials --------------------------------------------
  # soil provided as argument -> create soil nodes and materials and add them to param.b90
  if (!is.null(soil)) {
    param.b90[c("soil_nodes","soil_materials" )] <- soil_to_param(soil, options.b90$imodel)
  }

  if (options.b90$imodel == "MvG") {
    param.b90$soil_materials <- param.b90$soil_materials[,list(mat,ths,thr,alpha,npar,ksat,tort,gravel)]
  } else {
    param.b90$soil_materials <- param.b90$soil_materials[,list(mat,thsat,thetaf,psif,bexp,kf,wetinf,gravel)]
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

  if (!is.data.table(param.b90$soil_nodes)) {
    setDT(param.b90$soil_nodes)
  }

  if (!is.data.table(param.b90$soil_materials)) {
    setDT(param.b90$soil_materials)
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
      output = output,
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
    simres$finishing_time <- Sys.time()
    simres$simulation_duration <- simtime

  } else { #'dry' run
    simres <- list(model_input = list(options.b90 = options.b90,
                   param.b90 = param.b90,
                   standprop_daily = data.table(standprop_daily)))
    return(simres)
  }

  # append model input
  if (rtrn.input == TRUE){
    simres$model_input <- list(options.b90 = options.b90,
                               param.b90 = param.b90,
                               standprop_daily = data.table(standprop_daily))
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
                                            "prec.corr","prec.exposure","budburst.method",
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
          precip <- precip[, list(year(dates), month(dates), mday(dates), ii, prec, mesfl)]
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

