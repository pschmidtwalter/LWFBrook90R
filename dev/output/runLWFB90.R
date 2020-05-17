
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
                      output.log = TRUE,
                      run = TRUE,
                      verbose = TRUE,
                      ...) {


  # input checks ------------------------------------------------------------
  if (chk.input) {
    chk_options()
    chk_param()
    chk_clim()
    chk_soil()
    #chk_obs()
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
      output = output,
      output_log = output.log
    )

    finishing_time <- Sys.time()
    simtime <- finishing_time - start
    units(simtime) <- "secs"

    if (verbose == T) {
      message(paste("Simulation successful! Duration:", round(simtime,2), "seconds"))
    }

    # ---- process and manage outputs ---------------------------------------------------------------
    simout$day <- data.table::data.table(simout$day)
    data.table::setnames(simout$day, names(simout$day),
                         c('yr','mo','da','rfal','rint','sfal','sint','rsno','rnet','smlt','slfl',
                           'srfl','snow','swat','gwat','evap','tran','irvp','isvp','slvp','snvp',
                           'pint','ptran','pslvp','flow','seep','srfl','slfl','byfl','dsfl','gwfl',
                           'vrfln','nits','balerr'))

    simout$layer <- data.table::rbindlist(lapply(seq(dim(simout$layer)[3]),
                                                 function(x) data.frame(simout$layer[ , , x])),
                                  idcol = "nl")
    data.table::setnames(simout$layer, paste0("X", 1:15),
                         c('yr','mo','da','swati','theta','wetnes','psimi','psiti','infl',
                           'byfl','tran','slvp','vrfl','dsfl','ntfl'))

    simout$layer <- simout$layer[order(yr, mo, da, nl),]



    # ---- initialize return value ---------------------------------------------------------------
    simres <- list(simulation_duration = simtime,
                   finishing_time = finishing_time)

    # ---- append model input -------------------------------------------------------
    if (rtrn.input) {
      simres$model_input <- list(options.b90 = options.b90,
                                 param.b90 = param.b90,
                                 standprop_daily = standprop_daily)
    }

    # ---- append simuation results  -------------------------------------------------------
    simres[names(simout)] <- simout

    # ---- apply functions on simulation output -------------------------------
    if (!is.null(output_fun)) {
      if (verbose == T) {
        message("Applying function on simulation output files..")
      }
      if (!is.list(output_fun)){
        output_fun <- list(output_fun)
      }

      outfunargs <- list(x = simout,...)

      outfunargsnms <- lapply(output_fun, FUN = function(x,argsnms) {
        match.arg(methods::formalArgs(x),
                  argsnms,
                  several.ok = T)},
        argsnms = names(outfunargs))

      # TODO: simout is copied for use in each output_fun.
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
      simres <- simres[-which(names(simres) %in% names(simout))]
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





