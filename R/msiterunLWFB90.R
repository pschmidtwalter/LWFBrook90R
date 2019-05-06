#' Make a multi-site simulation using list of inputs.
#'
#' Repeatedly calls \code{\link[runLWFB90]{runLWFB90}} for lists of climate data, soil data,
#' parameter and options lists, stores the single run results in subdirectories and returns the results as a named list.
#'
#' @param param.b90 Named list of parameters to be used in all simulations, or list of lists with different parameter sets.
#' @param options.b90 Named list of model control option to be used in all simulations, or list of lists with different options sets.
#' @param soil data.frame with soil properties passed to \code{\link[runLWFB90]{runLWFB90}}, or a list of lists with different soil profiles
#' @param climate data.frame with climate data passed to \code{\link[runLWFB90]{runLWFB90}}, or a list of climate data.frames
#' @param all_combinations logical wether to set up all possible combinations of
#' param.b90, options, climate and soil lists. See details.
#' @param multirun.dir the directory where to create the subdirectories for the the single runs. Default 'MultiRuns/'
#' @param keep.subdirs keep sub-directories of the single runs? Default is FALSE
#' @param cores number of cores to use for parallel processing
#' @param showProgress Show progressbar?
#' @param ... Further arguments passed to \code{\link[runLWFB90]{runLWFB90}}.
#'
#' @return A named list with the results for the single runs. Simulation errors are passed on.
#'
#' @export

#' @examples

msiterunLWFB90 <- function(param.b90,
                           options.b90,
                           climate,
                           soil = NULL,
                           all_combinations = F,
                           multirun.dir = "MultiRuns/",
                           keep.subdirs = FALSE,
                           cores = 3,
                           showProgress = TRUE,
                           ...){


  #determine list lengths and setup the names
  param_len <- nstlist_length(param.b90)
  options_len <- nstlist_length(options.b90)
  soil_len <- nstlist_length(soil)
  clim_len <- nstlist_length(climate)

  if (all(c(param_len, options_len, clim_len, soil_len) == 1L)) {
    stop("No variation of inputs provided. Please any or all of param.b90, options.b90,
           soil or climate as a list of lists for a multi-site simulation.")
  }
  # ---- Prepare names and list structures for foreach-loop
  make_nms_climsoilparmopts()

  # ---- set up multirun-combinations climate, soil, param, options
  combinations <- setup_combinations(param_len,options_len,soil_len,clim_len,
                                     all_combinations = all_combinations)

  nRuns <- nrow(combinations)

  # set up multirun-directory
  multirun.dir <- normalizePath(multirun.dir, mustWork = F)
  if (!dir.exists(multirun.dir)) {
    dir.create(multirun.dir)
  }

  #set up Cluster and progressbar --------------------------------------------------

  # define local foreach symbols
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

  # outer loop iterates over climate to save memory -> result is nested
  results <- foreach::foreach(thisclim = iterators::iter(climate),
                              clim_no = iterators::icount(),
                              thisname = iterators::iter(clim_nms), #use thisname for proj.dir
                              .final = function(x) setNames(x, clim_nms),
                              .errorhandling = "pass",
                              .options.snow = opts) %:%
    # inner loop iterates over the combinations using thisclim
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
  #set names of list level 2: didnt work with .final i foreach, due to soil-name
  for (clim_no in 1:clim_len) {
    names(results[[clim_no]]) <- trimws(paste(soil_nms[combinations$soil[which(combinations$clim == clim_no)]],
                                              param_nms[combinations$param[which(combinations$clim == clim_no)]],
                                              options_nms[combinations$options[which(combinations$clim == clim_no)]]))
  }
  # unnest
  results <- unlist(results, recursive = F)

  return(results)
}

nstlist_length <- function(ll) {
  if (any(sapply(ll, is.list))) {
    len <- length(ll)
  } else {
    len <- 1L
  }
  return(len)
}

make_nms_climsoilparmopts <- function() {
  eval.parent(quote({
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
      if (is.data.frame(soil)) {soil <- list(soil)}
    }

    if (clim_len > 1) {
      if (is.null(names(climate))) {
        clim_nms <- as.character(1:clim_len)
      } else {clim_nms <- names(climate)}
    } else {
      clim_nms <- "1" # needed for iterator
      if (is.data.frame(climate)) { climate <- list(climate) }
    }
  }))
}





