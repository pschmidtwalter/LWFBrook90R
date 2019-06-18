#' Replace elements in a data.frame or vector of length > 1 by name
#'
#' @param x vector or dataframe
#' @param varnms variable names to match: Specify
#' @param vals values to insert
#'
#' @return The vector or data.frame in x with the elements 'varnms' replaced by vals.
#' @export
#'
#' @examples
#' soil_materials <- data.frame(ths = rep(0.4,3), alpha = rep(23.1, 3))
#'
#' varnms = c("soil_materials.ths3", "soil_materials.ths1", "soil_materials.alpha2")
#' vals = c(0.999, 0.001, 99)
#' soil_materials
#' replace_vecelements(soil_materials, varnms, vals)
#'
#' x <- setparam_LWFB90()[["pdur"]]
#' varnms <- c("pdur2", "pdur12")
#' vals <- c(0,10)
#' x
#' replace_vecelements(x, varnms, vals)
#' @importFrom utils stack unstack
#' @importFrom stats ave
replace_vecelements <- function(x, varnms, vals) {
  if (is.data.frame(x)) {
    varnms <- unlist(strsplit(varnms, split = ".", fixed  =T))[2*(1:length(varnms))]
    vals <- vals[order(varnms)]
    varnms <- varnms[order(varnms)]
    x_m <- stack(x)
    x_m$ind <- as.character(x_m$ind)
    x_m$var <- paste0(x_m$ind,ave(x_m$ind,x_m$ind, FUN = seq_along))
    stopifnot( identical( length(x_m$values[which(x_m$var %in% varnms)]),
                          length(vals)) )
    x_m <- x_m[order(x_m$var),]
    x_m$values[which(x_m$var %in% varnms)] <- vals
    unstack(x_m, values~ind)
    } else {
    x[as.integer(gsub("[^[:digit:].]", "",  varnms))] <- vals
    x
  }
}
