#' Split up soil into materials and soil nodes.
#'
#' @param soil Data.frame with soil layer boundaries ('upper', 'lower') and
#' hydraulic parameters. When imodel = 'MvG', columns of soil have to be named 'ths', 'thr',
#' 'alpha', 'npar', 'ksat', 'tort', 'gravel'. When imodel = 'CH', columns have to be named
#' thsat , 'thetaf','psif', 'bexp','kf', 'wetinf', 'gravel'.
#' @param imodel Name of the hydraulic model ('MvG' or 'CH')
#'
#' @return a list with data.frames 'soil_nodes' and 'soil_materials'
#' @export
#'
#' @examples
#' data(slb1_soil)
#' soil <- slb1_soil
#' soil <- cbind(soil, hydpar_wessolek_tab(soil$texture))
#' str(soil)
#'
#' soil_layers_materials <- soil_to_param(soil)
#' soil_layers_materials
soil_to_param <- function(soil, imodel="MvG") {

  ths<- NULL; thr<- NULL; alpha<- NULL; npar<- NULL; ksat<- NULL; tort<- NULL; gravel <- NULL;
  thsat<- NULL; thetaf<- NULL; psif<- NULL; bexp<- NULL; kf<- NULL; wetinf<- NULL; mat <- NULL;

  if (inherits(soil, 'data.table')) {
    soil_dt <- data.table::copy(soil)
  } else {
    soil_dt <- data.table::data.table(soil)
  }

  if (imodel == "MvG") {
    stopifnot(all(c("ths","thr","alpha","npar","ksat","tort","gravel") %in% names(soil_dt)))
    soil_dt[, mat := .GRP, by = list(ths, thr, alpha, npar, ksat, tort, gravel)]
    materials <- unique(soil_dt[,c("mat", "ths","thr","alpha","npar","ksat","tort","gravel")])
  } else {
    stopifnot(all(c("thsat","thetaf","psif","bexp","kf","wetinf","gravel") %in% names(soil_dt)))
    soil_dt[, mat := .GRP, by = list(thsat,thetaf,psif,bexp,kf,wetinf,gravel)]
    materials <- unique(soil_dt[,c("mat","thsat","thetaf","psif","bexp","kf","wetinf","gravel")])
  }
  soil_dt$thick <- soil_dt$upper - soil_dt$lower
  soil_dt$midpoint <- soil_dt$lower + soil_dt$thick/2
  soil_dt$thick <- round(soil_dt$thick * 1000) # mm
  soil_dt$layer <- 1:nrow(soil_dt)

  return(list(soil_nodes = soil_dt[,c("layer", "upper","lower","thick", "midpoint", "mat")],
              soil_materials = materials))
}
