#' Replace elements in a data.frame or vector of length > 1 by name
#'
#' @param x A vector or data.frame.
#' @param varnms Variable names to match: Specify position by name and index.
#' @param vals Vector of values to insert at the specified positions.
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
replace_vecelements <- function(x, varnms, vals) {

  if (is.data.frame(x)) {
    stopifnot(!anyNA(unlist(strsplit(varnms, split = ".", fixed  =T))[2*(1:length(varnms))]))
    varnms <- unlist(strsplit(varnms, split = ".", fixed  =T))[2*(1:length(varnms))]
    vals <- vals[order(varnms)]
    varnms <- varnms[order(varnms)]
    x_m <- utils::stack(x)
    x_m$ind <- as.character(x_m$ind)
    x_m$var <- paste0(x_m$ind, stats::ave(x_m$ind,x_m$ind, FUN = seq_along))
    stopifnot( identical( length(x_m$values[which(x_m$var %in% varnms)]),
                          length(vals)) )
    x_m <- x_m[order(x_m$var),]
    x_m$values[which(x_m$var %in% varnms)] <- vals
    utils::unstack(x_m, values~ind)
        } else {
    stopifnot(!anyNA(as.integer(gsub("[^[:digit:].]", "",  varnms))))
    x[as.integer(gsub("[^[:digit:].]", "",  varnms))] <- vals
    x
  }
}
