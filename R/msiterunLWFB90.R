#' Make a multi-site simulation using lists of climate, soil, and
#' parameter input objects.
#'
#' Wrapper function for \code{\link{runLWFB90}} to make multiple parallel
#' simulations of one or several parameter sets, for a series of sites with
#' individual climate and soil, or individual parameter sets for each
#' climate/soil combinations.
#'
#' @param options_b90 Named list of model control options to be used in all
#'   simulations
#' @param param_b90 Named list of parameters to be used in all simulations, or a
#'   list of multiple parameter sets.
#' @param soil Data.frame with soil properties to be used in all simulations, or
#'   a list of data.frames with different soil profiles.
#' @param climate Data.frame with climate data, or a list of climate
#'   data.frames. Alternatively, a function can be supplied that returns a
#'   data.frame. Arguments to the function can be passed via
#'   \code{climate_args}.
#' @param climate_args  List of named lists of arguments passed to
#'   \code{climate}, if this is a function.
#' @param all_combinations Logical: Set up and run all possible combinations of
#'   individual \code{param_b90}, \code{climate} and \code{soil} objects?
#'   Default is \code{FALSE}, running one object or the list of \code{param_b90}
#'   objects for a series of climate/soil combinations.
#' @param cores Number of cores to use for parallel processing.
#' @param showProgress Logical: Show progress bar? Default is \code{TRUE}. See
#'   also section \code{Progress bar} below.
#' @param ... Further arguments passed to \code{\link{runLWFB90}}.
#'
#' @return A named list with the results of the single runs as returned by
#'   \code{\link{runLWFB90}}. Simulation or processing errors are passed on. The
#'   names of the returned list entries are concatenated from the names of the
#'   input list entries in the following form: <climate> <soil> <param_b90>. If
#'   \code{climate} is a function, the names for <climate> are taken from the
#'   names of \code{climate_args}.
#'
#' @section Data management: The returned list of single run results can become
#'   very large, if many simulations are performed and the selected output
#'   contains daily resolution data sets, especially daily layer-wise soil
#'   moisture data. To not overload memory, it is advised to reduce the returned
#'   simulation results to a minimum, by carefully selecting the output, and
#'   make use of the option to pass a list of functions to
#'   \code{\link{runLWFB90}} (argument \code{output_fun}). These functions
#'   perform directly on the output of a single run simulation, and can be used
#'   for aggregating model output on-the-fly, or save results to a file or
#'   database. The regular output of \code{\link{runLWFB90}} can be suppressed
#'   by setting \code{rtrn.output = FALSE}, for exclusively returning the output
#'   of such functions. To provide full flexibility, the names of the current
#'   \code{soil}, \code{param_b90}, and \code{climate} are automatically passed
#'   as additional arguments (\code{soil_nm}, \code{param_nm},\code{clim_nm}) to
#'   \code{\link{runLWFB90}} and in this way become available to functions
#'   passed via \code{output_fun}. In order to not overload the memory with
#'   simulation input data, it is advised to provide a function instead of a
#'   list of \code{climate} data.frames, and specify its arguments for
#'   individual sites in \code{climate_args}, in case many sites with individual
#'   climates will be simulated.
#'
#' @section Progress bar: This function provides a progress bar via the package
#'   \CRANpkg{progressr} if \code{showProgress=TRUE}. The parallel computation
#'   is then wrapped with \code{progressr::with_progress()} to enable progress
#'   reporting from distributed calculations. The appearance of the progress bar
#'   (including audible notification) can be customized by the user for the
#'   entire session using \code{progressr::handlers()} (see
#'   \code{vignette('progressr-intro')}).
#'
#' @export
#'
#' @example inst/examples/msiterunLWFB90-help.R
#'
msiterunLWFB90 <- function(param_b90,
                           options_b90,
                           soil = NULL,
                           climate,
                           climate_args = NULL,
                           all_combinations = FALSE,
                           cores = 2,
                           showProgress = TRUE,
                           ...){

  if(cores > future::availableCores())
    stop(paste("Can not run on", cores, "cores! Only", future::availableCores(),
               "available."))

  # to pass CRAN check notes
  clim_nms <- NULL; soil_nms <- NULL; param_nms <- NULL; clim_no <- NULL;
  thisclim <- NULL; i <- NULL;
  `%do%` <- foreach::`%do%`
  `%:%` <- foreach::`%:%`

  #determine list lengths and setup the names
  param_len <- nstlist_length(param_b90)
  soil_len <- nstlist_length(soil)
  if (is.function(climate)) {
    clim_len <- nstlist_length(climate_args)
  } else {
    clim_len <- nstlist_length(climate)
  }

  if (all(c(param_len, clim_len, soil_len) <= 1L)) {
    stop("No variation of inputs provided. Please provide at least one of param_b90,
           soil or climate as a list for a multi-site simulation.")
  }

  # Prepare names and list structures for foreach-loop
  make_nms_climsoilparmopts()

  # check climate-function for missing arguments
  if (is.function(climate)) {
    # if (!all(methods::formalArgs(climate) %in% names(climate_args[[1]]))) {
    #   stop("Missing arguments for 'climate'-function in the list of variable arguments as specified in 'climate_args' ")
    # }
    if (!all(names(climate_args[[1]]) %in% methods::formalArgs(climate))) {
      stop("Some elements in the 'climate_args' list of arguments for 'climate'-function are unknown.")
    }
  }

  # ---- set up multirun-combinations climate, soil, param, options
  combinations <- setup_combinations(param_len,soil_len,clim_len,
                                     all_combinations = all_combinations)

  nRuns <- nrow(combinations)

  # setup names
  # names(results) <- trimws(paste(clim_nms[combinations$clim],
  #                                soil_nms[combinations$soil],
  #                                param_nms[combinations$param]))


  # set up Cluster and foreach-Loop ------------------------------------------
  # plan multisession does makeClusterPSOCK() in the background
  # cluster is stopped automagically on exit
  # a future plan that might have existed before will be restored on exit
  oplan <- future::plan(future::multisession, workers=cores)
  on.exit(future::plan(oplan), add=TRUE)
  doFuture::registerDoFuture()

  # set up iterator for climate
  if (is.function(climate)) {
    clim_iter <- fct_iter(f = climate, args = climate_args)
  } else {
    clim_iter <- iterators::iter(climate)
  }

  foreach_loop <- function(){

    i <- NULL

    # outer loop iterates over climate to save memory -> result is nested
    foreach::foreach(
      thisclim = clim_iter,
      clim_no = iterators::icount(),
      .final = function(x) stats::setNames(x, clim_nms),
      .errorhandling = "pass") %:%

      # inner loop iterates over the combinations in thisclim
      foreach::foreach(
        i = 1:nrow(combinations[which(combinations$clim == clim_no), ]),
        .errorhandling = "pass") %do% {

          #subset for readability
          combi_thisclim <- combinations[which(combinations$clim == clim_no), ]


          res <- runLWFB90(options_b90 = options_b90,
                           param_b90 = param_b90[[combi_thisclim$param[i]]],
                           soil = soil[[combi_thisclim$soil[i]]],
                           climate = thisclim,
                           soil_nm = soil_nms[combi_thisclim$soil[i]],
                           param_nm = param_nms[combi_thisclim$param[i]],
                           clim_nm = clim_nms[clim_no],
                           ...)

          increment_progressbar()

          return(res)
        }
  }

  # with progress bar if desired
  if(isTRUE(showProgress)){
    progressr::with_progress({
      increment_progressbar <- progressr::progressor(steps=nRuns)
      results <- foreach_loop()
    })
  } else {
    increment_progressbar <- function(){}
    results <- foreach_loop()
  }


  # unnest results
  results <- unlist(results, recursive = F)

  # set names
  names(results) <- trimws(paste(clim_nms[combinations$clim],
                                 soil_nms[combinations$soil],
                                 param_nms[combinations$param]))

  return(results)
}



#------------------------------------------------------------------------------
# Internal Helper Functions
#------------------------------------------------------------------------------

nstlist_length <- function(ll) {
  if (any(sapply(ll, is.list))) {
    len <- length(ll)
  } else {
    len <- 0L
  }
  return(len)
}

fct_iter <- function (f, args)
{
  xit <- iterators::iter(args)
  nextEl <- function() {
    do.call(f, iterators::nextElem(xit))
  }
  it <- list(nextElem = nextEl)
  class(it) <- c("abstractiter", "iter")
  it
}

make_nms_climsoilparmopts <- function() {

  eval.parent(quote({

    if (param_len > 0) {
      if (is.null(names(param_b90))) {
        param_nms <- as.character(1:param_len)
      } else {param_nms <- names(param_b90)}
    } else {
      param_nms <- NULL
      param_b90 <- list(param_b90) # lift up one level for foreach loop
      param_len <- 1L
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
      if (is.function(climate)) {
        if (is.null(names(climate_args))) {
          clim_nms <- as.character(1:clim_len)
        } else {clim_nms <- names(climate_args)}
      } else {
        if (is.null(names(climate))) {
          clim_nms <- as.character(1:clim_len)
        } else {clim_nms <- names(climate)}
      }
    } else {
      if (is.function(climate)) {
        climate_args <- list(climate_args)
      } else {
        climate <- list(climate)
      }
      clim_len <- 1L
      clim_nms <- NULL
    }
  }))
}

setup_combinations <- function(param_len, soil_len, clim_len,
                               all_combinations =FALSE){

  if (all_combinations) {
    combinations <- data.frame(expand.grid(param = 1:param_len,
                                           soil = 1:soil_len,
                                           climate = 1:clim_len))
  } else {

    if ( !identical(soil_len, clim_len) ) {
      if ( all(c(soil_len, clim_len) > 1) ) {
        stop("The lists of data.frames containing soil and climate have unequal length, both greater 1.
             Please either use one climate per soil, or many climates with the same soil, or vice versa.
             In order to set up all possible combinations of climate, soil, and parameters, please set all_combinations = TRUE")
      }

      soilclim_combi <- as.matrix(expand.grid(soil = 1:soil_len, climate = 1:clim_len))

    } else {
      soilclim_combi <- matrix( c(1:soil_len, 1:clim_len),
                                ncol = 2,dimnames = list(NULL, c("soil","clim")))
    }


    if (param_len != max(c(soil_len, clim_len))) {
      # repeat parameter sets for each soil/clim-combination
      combinations <- data.frame(param = rep(1:param_len, nrow(soilclim_combi)),
                                 matrix( rep(soilclim_combi, each = param_len),
                                         ncol = 2, dimnames = list(NULL, c("soil","clim"))))
    } else {
      # parameter sets along climate and/or soil
      combinations <- data.frame(param = 1:param_len,
                                 soilclim_combi)

    }
  }
  return(combinations)
}
