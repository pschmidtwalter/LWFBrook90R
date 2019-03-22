#' Set up combinations of parameters, options, climate and soil for mrunLWFB90
#'
#' @param param_len number of different parameters sets
#' @param options_len number of different option sets
#' @param soil_len number of different soil profiles
#' @param clim_len number of different climate data.tables
#' @param all_combinations set up all possible combinations? Defaults to FALSE.
#'
#'
#' @return a data.frame with indexes of which object to combine with which object.

#' @export
#'
#' @examples
setup_combinations <- function(param_len, options_len, soil_len, clim_len,
                               all_combinations =F){

  if (all_combinations) {
    combinations <- data.frame(expand.grid(param = 1:param_len, options = 1:options_len,
                                           soil = 1:soil_len, climate = 1:clim_len))
  } else {


    if ( !identical(param_len, options_len) ) {

      if ( all(c(param_len, options_len) > 1) ) {
        stop("The list of lists conaining the options and parameters have unequal length, both greater 1.
           Please either use one set of options per parameter set, or many parameter sets with the same options. In order to simulate all possible
           combinations of climate, soil, parameters and options, please set all_combinations = TRUE")
      }

      paramoptions_combi <- as.matrix(expand.grid(param = 1:param_len, options = 1:options_len))


    } else {
      paramoptions_combi <- matrix( c(1:param_len,1:options_len),
                                    ncol = 2, dimnames = list(NULL, c("param","options")))
    }
    if ( !identical(soil_len, clim_len) ) {
      if ( all(c(soil_len, clim_len) > 1) ) {
        stop("The lists of data.frames conaining soil and climate have unequal length, both greater 1.
             Please either use one climate per soil, or many climate with the same soils. In order to set up all possible
             combinations of climate, soil, parameters and options, please set all_combinations = TRUE")
      }
      soilclim_combi <- as.matrix(expand.grid(soil = 1:soil_len, climate = 1:clim_len))
      } else {
        soilclim_combi <- matrix( c(1:soil_len, 1:clim_len),
                                  ncol = 2,dimnames = list(NULL, c("soil","clim")))
      }
    combinations <- data.frame(param = rep(paramoptions_combi[,1], nrow(soilclim_combi)),
                               options = rep(paramoptions_combi[,2], nrow(soilclim_combi)),
                                             matrix( rep(soilclim_combi, each = nrow(paramoptions_combi)),
                                       ncol = 2, dimnames = list(NULL, c("soil","clim")))
                               )
  }
  return(combinations)
}

