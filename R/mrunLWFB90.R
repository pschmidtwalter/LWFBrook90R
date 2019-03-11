#' Make a multirun simulation using a set of variable input parameters.
#'
#' Repeatedly calls \code{\link[Run.B90]{Run.B90}} with varying input parameters in parallel,
#' stores the single run results in subdirectories and returns the results as a named list.
#'
#'
#' @param nRuns Number of single runs
#' @param param_var data.frame of variable input parameters with realisations for each single run.
#' @param param.b90 Named list of constant model input parameters to use in all single runs.
#' @param options.b90 Named list of constant model control options passed to \code{\link[Run.B90]{Run.B90}}
#' @param soil data.frame with soil properties passed to \code{\link[Run.B90]{Run.B90}}
#' @param climate data.frame with climate data passed to \code{\link[Run.B90]{Run.B90}}
#' @param param_var_names names of the parameters in param_var to be replaced in param.b90
#' @param multirun.dir The directory where to create the subdirectories for the the single runs.
#' @param singlerun_names character vector with the names of the single runs. Used for creating
#' the subdirectories and for naming the returned list of singlerun results.
#' @param cores Number of cores to use for parallel processing
#' @param showProgress Show progressbar?
#' @param ... Further arguments passed to \code{\link[Run.B90]{Run.B90}}.
#'
#' @return A named list with the results for the single runs. Simulation errors are passed on.
#' @export
#'
#' @examples
#' #Set up lists containing model control options and model parameters:
#' param.b90 <- MakeParam.B90()
#' options.b90 <- MakeOptions.B90()
#'
#' # Derive soil hydraulic properties from soil physical properties using pedotransfer functions
#' soil <- cbind(soil_slb1, hydpar_wessolek_mvg(soil_slb1$texture))
#'
#' # Set start and end Dates for the simulation
#' options.b90$startdate <- as.Date("2000-01-01")
#' options.b90$enddate <- as.Date("2004-12-31")
#'
#' # choose the 'Coupmodel' shape option for the annual lai dynamic, with fixed budburst and leaf fall dates:
#' options.b90$annuallaidyn <- 'Coupmodel'
#' options.b90$budburst <- 'fixed'
#' options.b90$leaffall <- 'fixed'
#'
#' #set up data.frame with variable leaf area index parameters
#' n <- 10
#' vary_parms <- data.frame(maxlai = runif(n,4,8),
#'                          shapestart = runif(n, 0.1,1),
#'                          winlaifrac = runif(n, 0,0.5),
#'                          budburstdoy = runif(n,100,150),
#'                          optdoy =runif(n,180,240))
#'
#' # Make a Multirun-Simulation
#' b90.multi <- MultiRun.B90(nRuns = n,
#'                      param_var = vary_parms,
#'                      param_const = param.b90,
#'                      options.b90 = options.b90,
#'                      soil = soil,
#'                      climate = meteo_slb1,
#'                      multirun.dir = "MultiRuns",
#'                      keep.subdirs = FALSE,
#'                      singlerun_names = paste0("result.",1:n),
#'                      cores = 3
#'                      )
mrunLWFB90 <- function(nRuns = nrow(param_var),
                         param_var,
                         param.b90,
                         options.b90,
                         soil,
                         climate,
                         param_var_names = names(param_var),
                         multirun.dir = "MultiRuns",
                         keep.subdirs = FALSE,
                         singlerun_names = paste0("RunNo",1:nRuns),
                         cores = 3,
                         showProgress = TRUE,
                         ...){

  param_var <- as.data.frame(param_var)
  names(param_var) <- param_var_names

  if (!requireNamespace("doSNOW", quietly = TRUE)) {
    stop("Package \"doSNOW\" needed for this function to work. Please install it.")
  }

  if (!all(names(param_var) %in% names(param.b90))) {
    stop( paste( "Not all names of 'param_var' were found in 'param_const'! Check names:",
                  names(param_var[which(!names(param_var) %in% names(param.b90))]) ) )
  }

  if (nrow(param_var) > nRuns) {
    warning(paste("Number of rows in 'param_var' is greater than 'nRuns'.
                  Only the first 'nRuns' =", nRuns, "rows of 'param_var' will be used in the Multirun!"))
  }
  if (nrow(param_var) < nRuns) {
    stop("The number of paramter sets is lower than 'nRun'.
         Please reduce 'nRuns' to the number of rows in 'param_var'!")
  }

  multirun.dir <- normalizePath(multirun.dir, mustWork = F)
  if (!dir.exists(multirun.dir)) {
    dir.create(multirun.dir)
  }

  # oldwd <- getwd()
  # setwd(multirun.dir)
  # on.exit(setwd(oldwd))

  #TODO make some manipulation on the vary parms: should work as dataframe and also as list,
  # to be able to input parameters with length > 1!

  #set up Cluster and progressbar --------------------------------------------------

  # define local %dopar% symbol, to not load foreach package (listed only under 'suggests')
  `%dopar%` <- foreach::`%dopar%`
  #`%do%` <- foreach::`%do%`

  progress <- function(nRuns) setTxtProgressBar(pb, nRuns)

  if (showProgress) {
    opts <- list(progress = progress)
  } else {
    opts <- list(progress = NULL)
  }

  cl <- snow::makeSOCKcluster(cores)
  doSNOW::registerDoSNOW(cl)
  snow::clusterEvalQ(cl, library("brook90r"))
  on.exit(snow::stopCluster(cl), add = T)

  pb <- txtProgressBar(min = 1, max = nRuns, style = 3)

  # foreach-Loop --------------------------------------------------------------------
  results <- foreach::foreach(i = seq_along(singlerun_names),
                              .errorhandling = "pass",
                              .options.snow = opts) %dopar% {

                                param.b90[match(names(param_var),names(param.b90))] <- param_var[i,]

                                res <- Run.B90(project.dir = file.path(multirun.dir, singlerun_names[i]),
                                               param.b90 = param.b90,
                                               options.b90 = options.b90,
                                               climate = climate,
                                               soil = soil,
                                               ...)

                                if (!keep.subdirs) {
                                  unlink(file.path(multirun.dir, singlerun_names[i]), recursive = TRUE)
                                }

                                return(res)
                              }
  names(results) <- singlerun_names
  return(results)
  }
