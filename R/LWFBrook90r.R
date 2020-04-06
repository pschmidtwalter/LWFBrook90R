#' LWFBrook90R: A package for running the LWF-BROOK90 hydrological model from within R
#'
#' The central function runLWFB90() creates model input  from model control options, parameters,
#' climate and soil data, executes the model code and returns the model results.
#' The model control options thereby let the user select different methods for defining
#' phenology and leaf area index seasonality, root density depth distributions,
#' and inter-annual variation of stand height and stem area index variation.
#' Additionally, a set of pedotransfer functions is provided to derive hydraulic
#' parameters from soil physical properties. For an introduction to the basic usage see \code{vignette('intro_lwfbrook90r')}.
#'
#' @docType package
#' @name LWFBrook90R
NULL
