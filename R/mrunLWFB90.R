#' Make a multirun simulation using a set of variable input parameters.
#'
#' Wrapper function for \code{\link{run_LWFB90}} to make multiple simulations
#' parallel, with varying input parameters.
#'
#' @param paramvar Data.frame of variable input parameters. For each row, a
#'   simulation is performed, with the elements in \code{param_b90} being
#'   replaced by the respective column of \code{paramvar}. All parameter names
#'   (column names) in \code{paramvar} must be found in \code{param_b90}. See
#'   section \code{Parameter updating}.
#' @param param_b90 Named list of parameters, in which the parameters defined in
#'   \code{paramvar} will be replaced.
#' @param paramvar_nms Names of the parameters in \code{paramvar} to be replaced
#'   in \code{param_b90}.
#' @param cores Number of CPUs to use for parallel processing. Default is 2.
#' @param showProgress Logical: Show progress bar? Default is TRUE. See also
#'   section \code{Progress bar} below.
#' @param ... Additional arguments passed to \code{\link{run_LWFB90}}: provide at
#'   least the arguments that have no defaults such as \code{climate}). It might
#'   be a good idea to also pass \code{verbose=FALSE} to suppress excessive
#'   chatter of \code{run_LWFB90}.
#'
#' @return A named list with the results of the single runs as returned by
#'   \code{\link{run_LWFB90}}. Simulation or processing errors are passed on.
#'
#' @section Parameter updating: The transfer of values from a row in
#'   \code{paramvar} to \code{param_b90} before each single run simulation is
#'   done by matching names from \code{paramvar} and \code{param_b90}. In order
#'   to address data.frame or vector elements in \code{param_b90} by a column
#'   name in \code{paramvar}, the respective column name has to be set up from
#'   its name and index in \code{param_b90}. To replace, e.g., the 2nd value of
#'   \code{ths} in the \code{soil_materials} data.frame, the respective column
#'   name in \code{paramvar} has to be called 'soil_materials.ths2'. In order to
#'   replace the 3rd value of \code{maxlai} vector in \code{param_b90}, the
#'   column has to be named 'maxlai3'.
#'
#' @section Data management: The returned list of single run results can become
#'   very large, if many simulations are done and the selected output contains
#'   daily resolution datasets, and especially daily layer-wise soil moisture
#'   data. To not overload memory, it is advised to reduce the returned
#'   simulation results to a minimum, by carefully selecting the output, and
#'   make use of the option to pass a list of functions to
#'   \code{\link{run_LWFB90}} via argument \code{output_fun}. These functions
#'   perform directly on the output of a single run simulation, and can be used
#'   for aggrating model output on-the-fly, or writing results to a file or
#'   database.
#'
#' @section Progress bar:
#' This function provides a progress bar via the package \CRANpkg{progressr}
#' if \code{showProgress=TRUE}. The parallel computation is then wrapped with
#' \code{progressr::with_progress()} to enable progress reporting from
#' distributed calculations. The appearance of the progress bar (including
#' audible notification) can be customized by the user for the entire session
#' using \code{progressr::handlers()} (see \code{vignette('progressr-intro')}).
#'
#' @export
#'
#' @example inst/examples/run_multi_LWFB90-help.R
#'
run_multi_LWFB90 <- function(paramvar,
                             param_b90,
                             paramvar_nms = names(paramvar),
                             cores = 2,
                             showProgress = TRUE,
                             ...){
  if(cores > future::availableCores())
    stop(paste("Can not run on", cores, "cores! Only", future::availableCores(),
               "available."))

  # to pass CRAN check Notes
  `%dopar%` <- foreach::`%dopar%`
  i <- NULL

  nRuns <- nrow(paramvar)

  # determine list and vector elements in param_b90
  is_ll <- lapply(param_b90, function(x) is.list(x) | length(x) > 1 )

  # which of the columns in paramvar belong to the list-parameters?
  param_ll <- sapply(names(param_b90[names(is_ll)[which(is_ll == TRUE)]]),
                     simplify = FALSE,
                     FUN =  grep,
                     x = paramvar_nms)

  # determine number of nonzero list entries
  param_ll_len <- length(param_ll[sapply(param_ll, function(x) length(x) > 0)])

  # check if all the names of paramvar can be found in param_b90
  if (param_ll_len > 0L) { #length > 1 includede in paramvar
    # remove zeros
    param_ll <- param_ll[sapply(param_ll, function(x) length(x) > 0)]
    singlepar_nms <- paramvar_nms[-unlist(param_ll)]
    nms <- c(names(param_ll), paramvar_nms[-unlist(param_ll)])
  } else {
    singlepar_nms <- paramvar_nms
    nms <- paramvar_nms
  }

  if (!all(nms %in% names(param_b90))) {
    stop( paste( "Not all names of 'paramvar' were found in 'param_b90'! Check names:",
                 paste(nms[which(!nms %in% names(param_b90))], collapse =", ") ))
  }


  # set up Cluster and foreach-Loop --------------------------------------------
  # plan multisession does makeClusterPSOCK() in the background
  # cluster is stopped automagically on exit
  # a future plan that might have existed before will be restored on exit
  oplan <- future::plan(future::multisession, workers=cores)
  on.exit(future::plan(oplan), add=TRUE)
  doFuture::registerDoFuture()

  foreach_loop <- function(){

    foreach::foreach(
      i = 1:nRuns,
      .final = function(x) stats::setNames(x, paste0("RunNo.", 1:nRuns)),
      .errorhandling = "pass",
      .export = "param_b90") %dopar% {

        # replace single value parameters
        param_b90[match(singlepar_nms, names(param_b90))] <- paramvar[i, match(singlepar_nms, paramvar_nms, nomatch = 0)]

        # replace parameters in data.frames and vector elements
        if (param_ll_len > 0) {
          for (l in 1:length(param_ll)){
            list_ind <- which(names(param_b90) == names(param_ll)[l])
            param_b90[[list_ind]] <- replace_vecelements(param_b90[[list_ind]],
                                                         varnms = paramvar_nms[param_ll[[l]]],
                                                         vals = unlist(paramvar[i, unlist(param_ll[[l]])]))
          }
        }

        # Run LWFBrook90
        res <- run_LWFB90(param_b90 = param_b90, ...)

        increment_progressbar()

        return(res)
      }
  }

  # with progressbar if wanted
  if(isTRUE(showProgress)){
    progressr::with_progress({
      increment_progressbar <- progressr::progressor(steps=nRuns)
      results <- foreach_loop()
    })
  } else {
    increment_progressbar <- function(){}
    results <- foreach_loop()
  }

  return(results)
}
