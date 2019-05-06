#' Make a multirun simulation using a variable input parameters.
#'
#' Repeatedly calls \code{\link[runLWFB90]{runLWFB90}} with varying input parameters in parallel,
#' stores the single run results in subdirectories and returns the results as a named list.
#'
#'
#' @param paramvar data.frame of variable input parameters with realisations for each single run.
#' The ith values replace the list elements in param.b90 of the ith simulation,
#' with the column names of paramvar being matched to the names in param.b90.
#' In order to match values from paramvar to data.frame or vector elements in param.b90,
#' the respective column name of paramvar has to be setup from the name and index.
#' For example, to place the 2nd value of 'ths' in the soil_materials data.frame,
#' the respective column name of paramvar has to be called 'soil_materials.ths2'.
#' In order to replace the 3 value of pdur in param.b90, the column name has to be named 'pdur3'.
#' The function for replacing values in vector element and data.frames is \code{\link[replace_vecelements]{replace_vecelements}}.
#' @param param.b90 Named list of parameters, in which the parameters defined in paramvar will be replaced.
#' @param options.b90 Named list of model control option to be used in all simulations.
#' @param soil data.frame with soil properties passed to \code{\link[runLWFB90]{runLWFB90}}, or a list of lists with different soil profiles
#' @param climate data.frame with climate data passed to \code{\link[runLWFB90]{runLWFB90}}, or a list of climate data.frames
#' @param paramvar_nms names of the parameters in paramvar to be replaced in param.b90
#' @param multirun.dir the directory where to create the subdirectories for the the single runs. Default 'MultiRuns/'
#' @param keep.subdirs keep sub-directories of the single runs? Default is FALSE
#' @param cores number of cores to use for parallel processing. Default is 3.
#' @param showProgress Show progressbar?
#' @param ... Further arguments passed to \code{\link[runLWFB90]{runLWFB90}}.
#'
#' @return A named list with the results for the single runs. Simulation errors are passed on.
#' @export

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

mrunLWFB90 <- function(paramvar,
                       param.b90,
                       options.b90,
                       climate,
                       soil = NULL,
                       paramvar_nms = names(paramvar),
                       multirun.dir = "MultiRuns/",
                       keep.subdirs = FALSE,
                       cores = 3,
                       showProgress = TRUE,
                       ...){

  nRuns <- nrow(paramvar)

  # determine list and vector elements in param.b90
  is_ll <- lapply(param.b90, function(x) is.list(x) | length(x) > 1 )

  # which of the columns in paramvar belong to the list-parameters?
  param_ll <- sapply(names(param.b90[names(is_ll)[which(is_ll == TRUE)]]),
                     simplify = F,
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
                 nms[which(!nms %in% names(param.b90))] ))
  }

  # set up multirun-directory -------------------------------------------------------
  multirun.dir <- normalizePath(multirun.dir, mustWork = F)
  if (!dir.exists(multirun.dir)) {
    dir.create(multirun.dir)
  }

  # set up local %dopar%
  `%dopar%` <- foreach::`%dopar%`

  # set up Cluster and progressbar --------------------------------------------------

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
  results <- foreach::foreach(i = 1:nRuns,
                              .final = function(x) setNames(x, paste0("RunNo.", 1:nRuns)),
                              .errorhandling = "pass",
                              .options.snow = opts) %dopar% {

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
                                                 options.b90 = options.b90,
                                                 soil = soil,
                                                 climate = climate,
                                                 ...)

                                if (!keep.subdirs) {
                                  unlink(file.path(proj.dir), recursive = TRUE)
                                }
                                return(res)
                              }

  return(results)
}





