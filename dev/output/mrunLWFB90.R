mrunLWFB90 <- function(paramvar,
                       param.b90,
                       paramvar_nms = names(paramvar),
                       cores = 2,
                       showProgress = TRUE,
                       ...){
  i <- NULL #to pass CRAN check Notes

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

  # set up local %dopar%
  `%dopar%` <- foreach::`%dopar%`

  # set up Cluster and progressbar --------------------------------------------------

  # progress <- function(nRuns) utils::setTxtProgressBar(pb, nRuns)
  #
  # if (showProgress) {
  #   opts <- list(progress = progress)
  # } else {
  #   opts <- list(progress = NULL)
  # }

  # cl <- snow::makeSOCKcluster(cores)
  # doSNOW::registerDoSNOW(cl)
  # snow::clusterEvalQ(cl, library("LWFBrook90R"))
  # on.exit(snow::stopCluster(cl), add = T)

  cl <- parallel::makeCluster(cores)
  #doParallel::registerDoParallel(cl)
  parallel::clusterEvalQ(cl, library("LWFBrook90R"))
  on.exit(parallel::stopCluster(cl), add = T)


  #pb <- utils::txtProgressBar(min = 1, max = nRuns, style = 3)

  # foreach-Loop --------------------------------------------------------------------
  results <- foreach::foreach(i = 1:nRuns,
                              .final = function(x) stats::setNames(x, paste0("RunNo.", 1:nRuns)),
                              .errorhandling = "pass"
                              ) %dopar% {

                                # replace single value parameters
                                param.b90[match(singlepar_nms,names(param.b90))] <- paramvar[i,match(singlepar_nms, paramvar_nms, nomatch = 0)]

                                # replace parameters in data.frames and vector elements
                                if (param_ll_len > 0) {
                                  for (l in 1:length(param_ll)) {
                                    list_ind <- which(names(param.b90) == names(param_ll)[l])
                                    param.b90[[ list_ind ]] <- replace_vecelements(param.b90[[ list_ind ]],
                                                                                   varnms = paramvar_nms[ param_ll[[l]] ],
                                                                                   vals = unlist(paramvar[i, unlist(param_ll[[l]])]))
                                  }
                                }

                                # Run LWFBrook90
                                runLWFB90(param.b90 = param.b90, ...)
                              }

  #  results <- parallel::parLapply(cl = cl, X = split(paramvar, 1:nRuns),
  #                                fun = function(X, param.b90, singlepar_nms, param_ll_len, param_ll) {
  #
  #   # replace single value parameters
  #   param.b90[match(singlepar_nms,names(param.b90))] <- X[1,match(singlepar_nms, paramvar_nms, nomatch = 0)]
  #
  #   # replace parameters in data.frames and vector elements
  #   if (param_ll_len > 0) {
  #     for (l in 1:length(param_ll)) {
  #       list_ind <- which(names(param.b90) == names(param_ll)[l])
  #       param.b90[[ list_ind ]] <- replace_vecelements(param.b90[[ list_ind ]],
  #                                                      varnms = paramvar_nms[ param_ll[[l]] ],
  #                                                      vals = unlist(X[1, unlist(param_ll[[l]])]))
  #     }
  #   }
  #
  #   # Run LWFBrook90
  #   runLWFB90(param.b90 = param.b90, ...)
  #
  # },
  # param.b90 = param.b90,
  # singlepar_nms = singlepar_nms,
  # param_ll_len = param_ll_len,
  # param_ll = param_ll,
  # options.b90,
  # ...)



  return(results)
}

f <- function(iterator){
  pb <- txtProgressBar(min = 1, max = iterator - 1, style = 3)
  count <- 0
  function(...) {
    count <<- count + length(list(...)) - 1
    setTxtProgressBar(pb, count)
    flush.console()
    cbind(...) # this can feed into .combine option of foreach
  }
}



