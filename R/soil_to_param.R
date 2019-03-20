#' Split up soil into materials and soil nodes.
#'
#' @param soil data.frame with soil soil depths ('upper', 'lower') and
#' hydraulic parameters ('ths', 'thr', 'alpha', 'npar', 'ksat', 'tort', 'gravel')
#'
#' @return a list with data.table objects 'soil_nodes' and 'soil_materials'
#' @export
#' @import data.table
#'
#' @examples
soil_to_param <- function(soil) {
  data.table::setDT(soil)
  dubl <- duplicated(soil[,c("ths","thr","alpha","npar","ksat","tort","gravel")])
  materials <- soil[!dubl,c("ths","thr","alpha","npar","ksat","tort","gravel")]
  materials$mat <- 1:nrow(materials)
  materials <- materials[,c("mat","ths","thr","alpha","npar","ksat","tort","gravel")] #Reihenfolge im Output
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

  return(list(soil_nodes = soil[,list(upper, lower, mat)],
              soil_materials = materials))
}
