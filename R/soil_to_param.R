#' Split up soil into materials and soil nodes.
#'
#' @param soil data.frame with soil depths ('upper', 'lower') and
#' hydraulic parameters. When imodel = 'MvG', columns of soil have to be named 'ths', 'thr',
#' 'alpha', 'npar', 'ksat', 'tort', 'gravel'. When imodel = 'CH', columns have to be named
#' thsat , 'thetaf','psif', 'bexp','kf', 'wetinf', 'gravel'.
#' @param imodel name of the hydraulic model
#'
#' @return a list with data.frames 'soil_nodes' and 'soil_materials'
#' @export
#'
#' @examples
#' soil <- slb1_soil
#' soil <- cbind(soil, hydpar_wessolek_mvg(soil$texture))
#' str(soil)
#'
#' soil_layers_materials <- soil_to_param(soil)
#' soil_layers_materials

soil_to_param <- function(soil, imodel="MvG") {

  if (imodel == "MvG") {
  dubl <- duplicated(soil[,c("ths","thr","alpha","npar","ksat","tort","gravel")])
  materials <- soil[!dubl,c("ths","thr","alpha","npar","ksat","tort","gravel")]
  } else {
    dubl <- duplicated(soil[,c("thsat","thetaf","psif","bexp","kf","wetinf","gravel")])
    materials <- soil[!dubl,c("thsat","thetaf","psif","bexp","kf","wetinf","gravel")]
  }
  materials$mat <- 1:nrow(materials)
  #add material-identifier to soil
  seqalong <- 2:length(dubl)
  soil$mat[1] <- 1
  m = 1
  for (i in seqalong) {
    if (dubl[i] == FALSE) {
      m = m + 1
      soil$mat[i] <- m
    } else soil$mat[i] <- m
  }

  soil$thick <- soil$upper - soil$lower
  soil$midpoint <- soil$lower + soil$thick/2
  soil$thick <- round(soil$thick * 1000) # mm
  soil$layer <- 1:nrow(soil)

  return(list(soil_nodes = soil[,c("layer", "upper","lower","thick", "midpoint", "mat")],
              soil_materials = materials))
}
