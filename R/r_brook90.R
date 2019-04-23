#' Define the function to run LWFBrook90 in R
#'
#' @param site a one row matrix with site level information
#' @param climate a matrix with 15 columns of climatic data
#' @param param a vector of parameters input.
#' @param paramYear a matrix of the yearly input parameters. Year column corresponds to one month.
#' @param materials a matrix of the materials parameters
#' @param soil a matrix of soil parameters
#' @param pfile a precipitation file
#' @param output a matrix of output settings
#'
#' @details This is the model
#'
#' @export
#' @useDynLib LWFBrook90R
#'
r_brook90 <- function(
  site,
  climate,
  param,
  paramYear,
  materials,
  soil,
  pfile = NULL,
  output,
  output_log = TRUE
  ){

  # Adjust the parameters vector based on the materials and soil matrix
  # param[ 1 ] <- nrow( climate )
  param[ 65 ] <- nrow( soil )
  param[ 66 ] <- nrow( materials )

  # make a matrix of precipitation fille
  if ( is.null(pfile) ){
    pfile <- matrix(-999, nrow = param[1] * site[[6]], ncol = 6)
  }

  # Run the model
  out <- .Fortran(
    'fbrook90',
    site = as.matrix( site, ncol = 6, nrow = 1),
    climate = as.matrix( climate, ncol = 15),
    param = as.vector(param),
    paramYear = as.matrix( paramYear, ncol = 12 ),
    materials = as.matrix( materials, ncol = 8 ),
    soil = as.matrix( soil, ncol = 6 ),
    pfile = as.matrix( pfile, ncol = 6),
    output = as.integer( as.matrix( output, ncol = 5, nrow = 10)),
    output_log = as.integer(output_log)
    )

  # return(NULL)
}
