#' Make a multirun simulation using a set of variable input parameters.
#'
#' Wrapper function for \code{\link{runLWFB90}} to make multiple simulations parallel,
#' with varying input parameters.
#'
#' @param paramvar Data.frame of variable input parameters. For each row,
#' a simulation is performed, with the elements in \code{param.b90} being replaced by the
#' respective column of \code{paramvar}. All parameter names (column names) in \code{paramvar} must be found in \code{param.b90}.
#' @param param.b90 Named list of parameters, in which the parameters defined in \code{paramvar} will be replaced.
#' @param paramvar_nms Names of the parameters in \code{paramvar} to be replaced in \code{param.b90}.
#' @param multirun.dir Directory name where to create the subdirectories for the single runs. Default is 'MultiRuns/'.
#' @param keep.subdirs Keep sub-directories of the single runs? Default is FALSE.
#' @param cores Number of CPUs to use for parallel processing. Default is 2.
#' @param showProgress Show progressbar? Default is TRUE. See also section \code{Progress bar}.
#' @param ... Additional arguments passed to \code{\link{runLWFB90}}:
#' provide at least the arguments that have no defaults (\code{options.b90} and \code{climate})!
#' It might be a good idea to also pass \code{verbose=FALSE} to supress excessive chatter of \code{runLWFB90}.
#'
#' @return A named list with the results of the single runs as returned by \code{\link{runLWFB90}}.
#' Simulation or processing errors are passed on.
#'
#' @section File management:
#' The LWF-Brook90 output files of the single runs are stored in subdirectories within 'multirun.dir'.
#' If \code{keep.subdirs=FALSE}, subdirectories are deleted after successful singlerun simulation. In case of an error,
#' the respective subdirectory is not deleted. The returned list of single run results can become very large,
#' if many simulations are done and the selected output contains daily resolution datasets, and especially daily layer-wise soil moisture data.
#' To not overload memory, it is advised to reduce the returned simulation results to a minimum, by carefully selecting the output,
#' and make use of the option to pass a list of functions to \code{\link{runLWFB90}} (argument \code{output_fun}). These functions
#' perform directly on the output of a single run simulation, and can be used for aggrating model output on-the-fly.
#'
#' @section Parameter updating:
#' The transfer of values from a row in paramvar to param.b90 before each single run
#' simulation is done by matching names from \code{paramvar} and \code{param.b90}. In order to adress data.frame
#' or vector elements in \code{param.b90} by a column name in \code{paramvar}, the respective column name
#' has to be setup from its name and index in \code{param.b90}. To replace, e.g., the 2nd value of \code{ths}
#' in the \code{soil_materials} data.frame, the respective column name in \code{paramvar}
#' has to be called 'soil_materials.ths2'. In order to replace the 3rd value of \code{maxlai} vector in \code{param.b90},
#' the column has to be named 'maxlai3'.
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
#' @examples
#' # Set up lists containing model control options and model parameters:
#' param.b90 <- setparam_LWFB90()
#' # choose the 'Coupmodel' shape option for the annual lai dynamic,
#' # with fixed budburst and leaf fall dates:
#' options.b90 <- setoptions_LWFB90(startdate = as.Date("2002-01-01"),
#'                                  enddate = as.Date("2003-12-31"),
#'                                  lai.method = 'Coupmodel',
#'                                  budburst.method = 'fixed',
#'                                  leaffall.method = 'fixed')
#'
#' # Derive soil hydraulic properties from soil physical properties using pedotransfer functions
#' soil <- cbind(slb1_soil, hydpar_wessolek_mvg(slb1_soil$texture))
#'
#' #set up data.frame with variable parameters
#' n <- 5
#' set.seed(2020)
#' vary_parms <- data.frame(shape.optdoy = runif(n,180,240),
#'                          shape.budburst = runif(n, 0.1,1),
#'                          winlaifrac = runif(n, 0,0.5),
#'                          budburstdoy = runif(n,100,150),
#'                          soil_materials.ths3 = runif(n, 0.3,0.5), #' ths of material 3
#'                          maxlai2 = runif(n,4,8)) #' lai in the 2nd year of the simulation
#'
#' # add the soil as soil_nodes and soil materials to param.b90, so ths3 can be looked up
#' param.b90[c("soil_nodes", "soil_materials")] <- soil_to_param(soil)
#'
#' # set up maxlai with length 2, so maxlai2 of paramvar can be looked up
#' param.b90$maxlai <- c(5, 5)
#'
#' # Make a Multirun-Simulation
#' b90.multi <- mrunLWFB90(paramvar = vary_parms,
#'                         param.b90 = param.b90,
#'                         options.b90 = options.b90,
#'                         climate = slb1_meteo)
#' names(b90.multi)
#'
#' # extract results: EVAPDAY.ASC
#' evapday <- data.frame(data.table::rbindlist(lapply(b90.multi,
#'                                                    FUN = function(x) {x[["EVAPDAY.ASC"]]}),
#'                                             idcol = "srun"))
#' evapday$dates <- as.Date(paste(evapday$YR, evapday$DOY),"%Y %j")
#'
#' srun_nms <- unique(evapday$srun)
#'
#' with(evapday[evapday$srun == srun_nms[1], ],
#'      plot(dates, cumsum(EVAP), type = "n",
#'           ylim = c(0,1000))
#' )
#' for (i in 1:length(b90.multi)){
#'   with(evapday[evapday$srun == srun_nms[i], ],
#'        lines(dates, cumsum(EVAP)))
#' }
mrunLWFB90 <- function(paramvar,
                       param.b90,
                       paramvar_nms = names(paramvar),
                       multirun.dir = "MultiRuns/",
                       keep.subdirs = FALSE,
                       cores = 2,
                       showProgress = TRUE,
                       ...){

  if(cores > future::availableCores())
    stop(paste("Can not run on", cores, "cores! Only", future::availableCores(),
               "available."))

  # to pass CRAN check Notes
  i <- NULL
  `%dopar%` <- foreach::`%dopar%`

  nRuns <- nrow(paramvar)

  # determine list and vector elements in param.b90
  is_ll <- lapply(param.b90, function(x) is.list(x) | length(x) > 1 )

  # which of the columns in paramvar belong to the list-parameters?
  param_ll <- sapply(names(param.b90[names(is_ll)[which(is_ll == TRUE)]]),
                     simplify = FALSE,
                     FUN =  grep,
                     x = paramvar_nms)

  # determine number of nonzero list entries
  param_ll_len <- length(param_ll[sapply(param_ll, function(x) length(x) > 0)])

  # check if all the names of paramvar can be found in param.b90
  if (param_ll_len > 0L) { #length > 1 includede in paramvar
    # remove zeros
    param_ll <- param_ll[sapply(param_ll, function(x) length(x) > 0)]
    singlepar_nms <- paramvar_nms[-unlist(param_ll)]
    nms <- c(names(param_ll), paramvar_nms[-unlist(param_ll)])
  } else {
    singlepar_nms <- paramvar_nms
    nms <- paramvar_nms
  }

  if (!all(nms %in% names(param.b90))) {
    stop( paste( "Not all names of 'paramvar' were found in 'param.b90'! Check names:",
                 paste(nms[which(!nms %in% names(param.b90))], collapse =", ") ))
  }

  # set up multirun-directory -------------------------------------------------------
  multirun.dir <- normalizePath(multirun.dir, mustWork = FALSE)
  if (!dir.exists(multirun.dir)) {
    dir.create(multirun.dir)
  }

  # set up Cluster and define foreach-Loop ------------------------------------------
  # plan multisession does makeClusterPSOCK() in the background
  # cluster is stoped automagically on exit
  # a future plan that might have existied before will be restored on exit
  oplan <- future::plan(future::multisession, workers=cores)
  on.exit(future::plan(oplan), add=TRUE)
  doFuture::registerDoFuture()

  foreach_loop <- function(){
    foreach::foreach(
      i = 1:nRuns,
      .final = function(x) stats::setNames(x, paste0("RunNo.", 1:nRuns)),
      .errorhandling = "pass",
      .export = "param.b90") %dopar% {

        # replace single value parameters
        param.b90[match(singlepar_nms,names(param.b90))] <- paramvar[i,match(singlepar_nms, paramvar_nms, nomatch = 0)]

        # replace parameters in data.frames and vector elements
        if (param_ll_len > 0) {
          for (l in 1:length(param_ll)){
            list_ind <- which(names(param.b90) == names(param_ll)[l])
            param.b90[[ list_ind ]] <- replace_vecelements(param.b90[[ list_ind ]],
                                                           varnms = paramvar_nms[ param_ll[[l]] ],
                                                           vals = unlist(paramvar[i, unlist(param_ll[[l]])]))
          }
        }
        # Set up directory name
        proj.dir <- file.path(multirun.dir,paste0("RunNo.",i))

        # Run LWFBrook90
        res <- runLWFB90(project.dir = proj.dir,
                         param.b90 = param.b90,
                         ...)

        increment_progressbar()

        if (!keep.subdirs) {
          unlink(file.path(proj.dir), recursive = TRUE)
        }
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
