#' Make a multirun simulation using a set of variable input parameters.
#'
#' Wrapper function for \code{\link{runLWFB90}} to make multiple simulations parallel,
#' with varying input parameters.
#'
#' @param paramvar data.frame of variable input parameters. For each row,
#' a simulation is performed, with the elements in param.b90 being replaced by the
#' respective column of paramvar. All parameter names (column names) in paramvar must be found in param.b90.
#' @param param.b90 Named list of parameters, in which the parameters defined in paramvar will be replaced.
#' @param options.b90 Named list of model control option to be used in all simulations.
#' @param soil data.frame with soil properties passed to \code{\link{runLWFB90}}, or a list of lists with different soil profiles
#' @param climate data.frame with climate data passed to \code{\link{runLWFB90}}, or a list of climate data.frames
#' @param paramvar_nms names of the parameters in paramvar to be replaced in param.b90
#' @param multirun.dir the directory where to create the subdirectories for the the single runs. Default is 'MultiRuns/'.
#' @param keep.subdirs keep sub-directories of the single runs? Default is FALSE.
#' @param cores number of CPUs to use for parallel processing. Default is 2.
#' @param showProgress Show progressbar? Default is TRUE.
#' @param ... Further arguments passed to \code{\link{runLWFB90}} to select model output
#' and return values for the single run simulations.
#'
#' @return A named list with the results of the single runs as returned by \code{\link{runLWFB90}}.
#' Simulation or processing errrors are passed on.
#'
#' @details
#' @section The LWF-Brook90 output files of the single runs are stored in subdirectories within 'multirun.dir'.
#' If \code{keep.subdirs=FALSE}, they are deleted after successful singlerun simulation. In case of an error,
#' the respective subdirectory is not deleted. Care must be taken, as the returned list of single run results can become very large,
#' if many simulations are done and the selected output contains daily resolution datasets.
#'
#' @section The transfer of values from a row in paramvar to param.b90 before each single run
#' simulation is done by matching names from paramvar and \code{param.b90}. In order to adress data.frame
#' or vector elements in \code{param.b90} with a column name in \code{paramvar}, the respective column name
#' has to be setup from its name and index in param.b90. For example, to place the 2nd value of \code{ths}
#' in the \code{soil_materials} data.frame, the respective column name in \code{paramvar}
#' has to be called 'soil_materials.ths2'. In order to replace the 3rd value of vector element \code{maxlai} in \code{param.b90},
#' the column name has to be named 'maxlai3'. The function used for replacing values in vector elements
#' and data.frames is \code{\link{replace_vecelements}}.
#'
#' @export
#'
#' @examples
#' # Set up lists containing model control options and model parameters:
#' param.b90 <- setparam_LWFB90()
#' options.b90 <- setoptions_LWFB90()
#'
#' # Derive soil hydraulic properties from soil physical properties using pedotransfer functions
#' soil <- cbind(slb1_soil, hydpar_wessolek_mvg(slb1_soil$texture))
#'
#' # Set start and end Dates for the simulation
#' options.b90$startdate <- as.Date("2002-01-01")
#' options.b90$enddate <- as.Date("2003-12-31")
#'
#' # choose the 'Coupmodel' shape option for the annual lai dynamic, with fixed budburst and leaf fall dates:
#' options.b90$lai.method <- 'Coupmodel'
#' options.b90$budburst.method <- 'fixed'
#' options.b90$leaffall.method <- 'fixed'
#'
#' # set up data.frame with variable parameters
#' n <- 5
#' vary_parms <- data.frame(shape.optdoy = runif(n,180,240),
#'                          shape.budburst = runif(n, 0.1,1),
#'                          winlaifrac = runif(n, 0,0.5),
#'                          budburstdoy = runif(n,100,150),
#'                          soil_materials.ths3 = runif(n, 0.3,0.5),
#'                          maxlai2 = runif(n,4,8))
#'
#' # soil as soil_nodes and soil materials to param.b90, so ths3 can be looked up
#' param.b90[c("soil_nodes", "soil_materials")] <- soil_to_param(soil)
#' # set up maxlai with length 2, so maxlai2 of paramvar can be looked up
#' param.b90$maxlai <- c(5, 5)
#'
#' \dontrun{
#' # Make a Multirun-Simulation (this takes a while)
#' b90.multi <- mrunLWFB90(paramvar = vary_parms,
#'                         param.b90 = param.b90,
#'                         options.b90 = options.b90,
#'                         climate = slb1_meteo)
#' names(b90.multi)
#'
#' # extract results: EVAPDAY.ASC
#' evapday <- data.table::rbindlist(lapply(b90.multi,
#'                                         FUN = function(x) {x[["EVAPDAY.ASC"]]}),
#'                                  idcol = "srun")
#' evapday$dates <- with(evapday, as.Date(paste(YR,MO,DA), "%Y %m %d"))
#'
#' with(evapday[evapday$srun == "RunNo.1", ],
#'      plot(dates, cumsum(EVAP)))
#' }
#'
#' # same simulation, but return goodnes-fit-measure only.
#' # prepare observations
#' observations <- slb1_mpot # daily water potential in different soil depths
#' # prepare data: names have to be found in simulation output.
#' names(observations)[2:6] <- c("psimi5", "psimi7", "psimi10", "psimi16","psimi21")
#'
#' \dontrun{
#' # run model, but only return gof-function results
#' b90.gofmpot <- mrunLWFB90(paramvar = vary_parms,
#'                           options.b90 = options.b90,
#'                           param.b90 = param.b90,
#'                           climate = slb1_meteo,
#'                           soil = soil,
#'                           obs = observations,
#'                           gof_fun = hydroGOF::NSE, #' Nash-Sutcliff efficiency
#'                           rtrn.output = FALSE,
#'                           rtrn.input = FALSE)
#' # collect gof from simulations results
#' nse <- data.table::rbindlist(lapply(b90.gofmpot, function(x) {as.list(x$gof)}),
#'                              idcol = "srun")
#' hist(nse$SWATDAY.ASC.psimi5)}

mrunLWFB90 <- function(paramvar,
                       param.b90,
                       options.b90,
                       climate,
                       soil = NULL,
                       paramvar_nms = names(paramvar),
                       multirun.dir = "MultiRuns/",
                       keep.subdirs = FALSE,
                       cores = 2,
                       showProgress = TRUE,
                       ...){

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





