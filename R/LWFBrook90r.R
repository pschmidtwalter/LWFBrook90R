#' LWFBrook90R: A package for running the LWF-BROOK90 hydrologic model in
#' R
#'
#' The central function \code{run_LWFB90()} creates model input  from model
#' control options, parameters, climate and soil data, executes the model code
#' and returns the model results. The model control options thereby let the user
#' select different methods for defining phenology and leaf area index
#' seasonality, root density depth distributions, and inter-annual variation of
#' stand properties. Additionally, a set of pedotransfer functions is provided
#' to derive hydraulic parameters from soil physical properties. For an
#' introduction to the basic usage see \code{vignette('intro_LWFB90')}.
#'
#' @docType package
#' @name LWFBrook90R
NULL
