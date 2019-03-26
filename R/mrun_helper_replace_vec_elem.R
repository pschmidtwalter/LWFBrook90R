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
#'
#'
#' x =param.b90[["soil_materials"]]
#' varnms = "soil_materials.ths1"
#' vals = 0.999
#' replace_vecelements(x, varnms, vals)

#' x <- param.b90[["pdur"]]
#' varnms <- c("pdur2", "pdur12")
#' vals <- c(0,10)
#' replace_vecelements(x, varnms, vals)
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
    x_m$values[which(x_m$var %in% varnms)] <- vals
    unstack(x_m, values~ind)
  } else {
    x[as.integer(gsub("[^[:digit:].]", "",  varnms))] <- vals
    x
  }
}
