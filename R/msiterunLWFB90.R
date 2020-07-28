#' Make a multi-site simulation using lists of climate, soil, options and parameter input objects.
#'
#' Wrapper function for \code{\link{runLWFB90}} to make multiple parallel simulations for combinations
#' of climate, soil, parameters and model control options, e.g., for simulating one  or sevral parameter sets
#' for a series of sites with individual climate and soil.
#'
#' @param param.b90 Named list of parameters to be used in all simulations, or a list of multiple parameter sets.
#' @param options.b90 Named list of model control options to be used in all simulations, or a list of multiple lists with different options.
#' @param soil Data.frame with soil properties to be used in all simulations, or a list of data.frames with different soil profiles.
#' @param climate Data.frame with climate data, or a list of climate data.frames.
#' @param all_combinations Logical: Set up and run all possible combinations of
#' individual param.b90, options.b90, climate and soil objects? Default is FALSE,
#' running one or multiple param.b90/options.b90 combinations for a series of climate/soil combinations.
#' @param multirun.dir Directory name where to create the subdirectories for the single runs. Default is 'MultiRuns/'.
#' @param keep.subdirs Keep sub-directories of the single runs after successful simulation? Default is FALSE.
#' @param cores Number of cores to use for parallel processing.
#' @param showProgress Logical: Show progressbar?
#' @param ... Further arguments passed to \code{\link{runLWFB90}}.
#'
#' @return A named list with the results of the single runs as returned by \code{\link{runLWFB90}}.
#' Simulation or processing errors are passed on. The names of the returned list entries
#' are concatenated from the names of the input list entries
#' in the following form: <climate> <soil> <param.b90> <options.b90>.
#'
#' @section Data management:
#' The returned list of single run results can become very large,if many simulations are done and
#' the selected output contains daily resolution datasets, and especially daily layer-wise soil moisture data.
#' To not overload memory, it is advised to reduce the returned simulation results to a minimum, by
#' carefully selecting the output, and make use of the option to pass a list of functions to
#' \code{\link{runLWFB90}} (argument \code{output_fun}). These functions perform directly on the
#' output of a single run simulation, and can be used for aggrating model output on-the-fly,
#' or writing results to a file or database.
#' @export
#'
#' @example inst/examples/msiterunLWFB90-help.R
msiterunLWFB90 <- function(param.b90,
                           options.b90,
                           climate,
                           soil = NULL,
                           all_combinations = F,
                           cores = 2,
                           showProgress = TRUE,
                           ...){

  clim_nms <- NULL; soil_nms <- NULL; param_nms <- NULL; options_nms <- NULL
  clim_no <- NULL; i <- NULL; thisname <- NULL; thisclim <- NULL

  #determine list lengths and setup the names
  param_len <- nstlist_length(param.b90)
  options_len <- nstlist_length(options.b90)
  soil_len <- nstlist_length(soil)
  clim_len <- nstlist_length(climate)

  if (all(c(param_len, options_len, clim_len, soil_len) == 1L)) {
    stop("No variation of inputs provided. Please provide at least one of param.b90, options.b90,
           soil or climate as a list for a multi-site simulation.")
  }
  # ---- Prepare names and list structures for foreach-loop
  make_nms_climsoilparmopts()

  # ---- set up multirun-combinations climate, soil, param, options
  combinations <- setup_combinations(param_len,options_len,soil_len,clim_len,
                                     all_combinations = all_combinations)

  nRuns <- nrow(combinations)

  #set up Cluster and progressbar --------------------------------------------------

  # define local foreach symbols
  `%dopar%` <- foreach::`%dopar%`
  `%:%` <- foreach::`%:%`

  progress <- function(nRuns) utils::setTxtProgressBar(pb, nRuns)

  if (showProgress) {
    opts <- list(progress = progress)
  } else {
    opts <- list(progress = NULL)
  }

  cl <- snow::makeSOCKcluster(cores)
  doSNOW::registerDoSNOW(cl)
  snow::clusterEvalQ(cl, library("LWFBrook90R"))
  on.exit(snow::stopCluster(cl), add = T)

  pb <- utils::txtProgressBar(min = 1, max = nRuns, style = 3)

  # foreach-Loop --------------------------------------------------------------------
  if (is.null(clim_nms)) {
    outer_nms <- "clim_1"
  } else { outer_nms <- clim_nms}

  # outer loop iterates over climate to save memory -> result is nested
  results <- foreach::foreach(thisclim = iterators::iter(climate),
                              clim_no = iterators::icount(),
                              thisname = iterators::iter(outer_nms), #use thisname for proj.dir
                              .final = function(x) stats::setNames(x, clim_nms),
                              .errorhandling = "pass",
                              .options.snow = opts) %:%

    # inner loop iterates over the combinations using thisclim
    foreach::foreach(i = 1:nrow(combinations[which(combinations$clim == clim_no),]),
                     .errorhandling = "pass") %dopar% {

                       #subset for readibility
                       combi_thisclim <- combinations[which(combinations$clim == clim_no),]

                       runLWFB90(project.dir = proj.dir,
                                 param.b90 = param.b90[[combi_thisclim$param[i]]],
                                 options.b90 = options.b90[[combi_thisclim$options[i]]],
                                 soil = soil[[combi_thisclim$soil[i]]],
                                 climate = thisclim,
                                 ...)
                     }

  # unnest
  results <- unlist(results, recursive = F)

  names(results) <- trimws(paste(clim_nms[combinations$clim],
                                 soil_nms[combinations$soil],
                                 param_nms[combinations$param],
                                 options_nms[combinations$options]))

  return(results)
}

nstlist_length <- function(ll) {
  if (any(sapply(ll, is.list))) {
    len <- length(ll)
  } else {
    len <- 0L
  }
  return(len)
}

make_nms_climsoilparmopts <- function() {

  eval.parent(quote({

    if (param_len > 0) {
      if (is.null(names(param.b90))) {
        param_nms <- as.character(1:param_len)
      } else {param_nms <- names(param.b90)}
    } else {
      param_nms <- NULL
      param.b90 <- list(param.b90) # lift up one level for foreach loop
      param_len <- 1L
    }

    if (options_len > 0) {
      if (is.null(names(options.b90))) {
        options_nms <- as.character(1:options_len)
      } else {options_nms <- names(options.b90)}
    } else {
      options_nms <- NULL
      options.b90 <- list(options.b90)
      options_len <- 1L
    }

    if (soil_len > 0) {
      if (is.null(names(soil))) {
        soil_nms <- as.character(1:soil_len)
      } else {soil_nms <- names(soil)}
    } else {
      soil_nms <- NULL
      soil <- list(soil)
      soil_len <- 1L

    }

    if (clim_len > 0) {
      if (is.null(names(climate))) {
        clim_nms <- as.character(1:clim_len)
      } else {clim_nms <- names(climate)}
    } else {
      clim_nms <- NULL
      climate <- list(climate)
      clim_len <- 1L
    }
  }))
}

setup_combinations <- function(param_len, options_len, soil_len, clim_len,
                               all_combinations =FALSE){

  if (all_combinations) {
    combinations <- data.frame(expand.grid(param = 1:param_len, options = 1:options_len,
                                           soil = 1:soil_len, climate = 1:clim_len))
  } else {

    if ( !identical(param_len, options_len) ) {

      if ( all(c(param_len, options_len) > 1) ) {
        stop("The lists of options.b90 and param.b90 objects have unequal length, both greater 1.
           Please either use one set of options per parameter set, or many parameter sets with one single options.b90 object, or vice versa.
           In order to simulate all possible combinations of climate, soil, parameters and options,
           please set all_combinations = TRUE")
      }

      paramoptions_combi <- as.matrix(expand.grid(param = 1:param_len, options = 1:options_len))


    } else {
      paramoptions_combi <- matrix( c(1:param_len,1:options_len),
                                    ncol = 2, dimnames = list(NULL, c("param","options")))
    }
    if ( !identical(soil_len, clim_len) ) {
      if ( all(c(soil_len, clim_len) > 1) ) {
        stop("The lists of data.frames containing soil and climate have unequal length, both greater 1.
             Please either use one climate per soil, or many climates with the same soil, or vice versa.
             In order to set up all possible combinations of climate, soil, parameters and options, please set all_combinations = TRUE")
      }
      soilclim_combi <- as.matrix(expand.grid(soil = 1:soil_len, climate = 1:clim_len))
    } else {
      soilclim_combi <- matrix( c(1:soil_len, 1:clim_len),
                                ncol = 2,dimnames = list(NULL, c("soil","clim")))
    }
    combinations <- data.frame(param = rep(paramoptions_combi[,1], nrow(soilclim_combi)),
                               options = rep(paramoptions_combi[,2], nrow(soilclim_combi)),
                               matrix( rep(soilclim_combi, each = nrow(paramoptions_combi)),
                                       ncol = 2, dimnames = list(NULL, c("soil","clim")))
    )
  }
  return(combinations)
}





