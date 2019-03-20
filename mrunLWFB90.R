

mrunLWFB90 <- function(param.b90,
                       options.b90,
                       soil,
                       climate,
                       param_var = NULL,
                       all_combinations = F,
                       multirun.dir = "MultiRuns",
                       keep.subdirs = FALSE,
                       cores = 3,
                       showProgress = TRUE,
                       ...){


  # 2 Options:
  # 1. param, opts, climate, soil are list of lists -> MultiSite-Run
  # 2. any of param or soil are matrices -> Calibration-Run ? Or wrapper? How to get the other parameters? setparamLWFB90()?


  if (is.matrix(param.b90)) {

    # if (!all(names(param_var) %in% names(param.b90))) {
    #   stop( paste( "Not all names of 'param_var' were found in 'param.b90'! Check names:",
    #                names(param_var[which(!names(param_var) %in% names(param.b90))]) ) )
    # }
    #


    #
    # nRuns <- nrow(param_var)
    # param_var <- as.data.frame(param_var)
    # names(param_var) <- param_var_names


  } else { # ---- length of the lists

    #determine list lengths and setup the names
    param_len <- nstlist_length(param.b90)
    options_len <- nstlist_length(options.b90)
    soil_len <- nstlist_length(soil)
    clim_len <- nstlist_length(climate)

    if (param_len > 1) {
      if (is.null(names(param.b90))) {
        param_nms <- as.character(1:param_len)
      } else {param_nms <- names(param.b90)}
    } else {
      param_nms <- NULL
      param.b90 <- list(param.b90) # lift up one level for foreach loop
    }

    if (options_len > 1) {
      if (is.null(names(options.b90))) {
        options_nms <- as.character(1:options_len)
      } else {options_nms <- names(options.b90)}
    } else {
      options_nms <- NULL
      options.b90 <- list(options.b90)
    }

    if (soil_len > 1) {
      if (is.null(names(soil))) {
        soil_nms <- as.character(1:soil_len)
      } else {soil_nms <- names(soil)}
    } else {
      soil_nms <- NULL
      if( is.data.frame(soil)) {soil <- list(soil)}
    }

    if (clim_len > 1) {
      if (is.null(names(climate))) {
        clim_nms <- as.character(1:clim_len)
      } else {clim_nms <- names(climate)}
    } else {
      clim_nms <- "1" # needed for iterator
      if (is.data.frame(climate)) { climate <- list(climate) }
    }

    if(all(c(param_len, options_len, clim_len, soil_len) == 1L & is.null(param_var))) {
      stop("No variation of inputs provided. Please set up any param.b90, options.b90, soil or climate as a list of lists for a multirun-simulation.")
    }

    # ---- set up multirun-combinations climate, soil, param, options

    combinations <- setup_combinations(param_len,options_len,soil_len,clim_len,
                                       all_combinations = all_combinations)

  }


  nRuns <- nrow(combinations)

  # set up multirun-directory
  multirun.dir <- normalizePath(multirun.dir, mustWork = F)
  if (!dir.exists(multirun.dir)) {
    dir.create(multirun.dir)
  }

  #set up Cluster and progressbar --------------------------------------------------

  # define local symbols
  `%dopar%` <- foreach::`%dopar%`
  `%:%` <- foreach::`%:%`

  progress <- function(nRuns) setTxtProgressBar(pb, nRuns)

  if (showProgress) {
    opts <- list(progress = progress)
  } else {
    opts <- list(progress = NULL)
  }
  #
  cl <- snow::makeSOCKcluster(cores)
  doSNOW::registerDoSNOW(cl)
  snow::clusterEvalQ(cl, library("LWFBrook90R"))
  on.exit(snow::stopCluster(cl), add = T)

  pb <- txtProgressBar(min = 1, max = nRuns, style = 3)

  # foreach-Loop --------------------------------------------------------------------
  #https://docs.microsoft.com/en-us/machine-learning-server/r/how-to-revoscaler-distributed-computing-foreach

  # outer iteration over climate to save memory
  results <- foreach::foreach(thisclim = iterators::iter(climate),
                              clim_no = iterators::icount(), thisname = iterators::iter(clim_nms), #use thisname for proj.dir
                              .final = function(x) setNames(x, clim_nms),
                              .errorhandling = "pass",
                              .options.snow = opts) %:%

    foreach::foreach(i = 1:nrow(combinations[which(combinations$clim == clim_no),]),
                     .errorhandling = "pass") %dopar% {

                       #subet for readibility
                       combi_thisclim <- combinations[which(combinations$clim == clim_no),]
                       #set project-directory
                       proj.dir <- file.path(multirun.dir, trimws(paste(thisname,
                                                                        soil_nms[combi_thisclim$soil[i]],
                                                                        param_nms[combi_thisclim$param[i]],
                                                                        options_nms[combi_thisclim$options[i]])))

                       res <- runLWFB90(project.dir = proj.dir,
                                        param.b90 = param.b90[[combi_thisclim$param[i]]],
                                        options.b90 = options.b90[[combi_thisclim$options[i]]],
                                        soil = soil[[combi_thisclim$soil[i]]],
                                        climate = thisclim,
                                        ...)

                       if (!keep.subdirs) {
                         unlink(file.path(proj.dir), recursive = TRUE)
                       }
                       return(res)
                     }
  #set names of list level 2: didnt work with .final, due to soil-name
  for (clim_no in 1:clim_len) {
    names(results[[clim_no]]) <- trimws(paste(soil_nms[combinations$soil[which(combinations$clim == clim_no)]],
                                              param_nms[combinations$param[which(combinations$clim == clim_no)]],
                                              options_nms[combinations$options[which(combinations$clim == clim_no)]]))
  }
  if (clim_len == 1) { # remove climate nesting if any
    results <- results[[1]]
  }

  return(results)
}

