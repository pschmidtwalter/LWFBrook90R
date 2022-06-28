#' Extracts values from layer data and organizes layer-wise variables in columns
#'
#' Convenience function to reorganize soil layer time series data from
#' \code{layer_output} list entry produced with \code{\link{run_LWFB90}}. The data is tansformed to a
#' wide format, by casting the variables with the layer number using data.table's
#' \code{\link[data.table]{dcast}}-function.
#'
#' @param x Data.frame or data.table with layer data organized in rows and
#'   identified by a layer index column named \code{layer_index_nm}.
#' @param layers Integer vector to select a subset of layers. If not supplied,
#'   values from all layers will be returned.
#' @param value_vars Character vector containing names of value-variables to be
#'   extracted from \code{x}. If not supplied, \code{value_vars} will be
#'   guessed.
#' @param layer_index_name Column containing layer index. Defaults to 'nl' as in
#'   \code{layer_output}.
#' @param sep Separation character for constructig names from variable name and
#'   layer index.
#'
#' @return A data.table with the layers' values of the variables organized in
#'   columns with the names being made up of the variable name and the layer index.
#' @export
#' @example inst/examples/extract_layer_output-help.R
#' @import data.table
extract_layer_output <- function(x,
                                 layers = NULL,
                                 value_vars = NULL,
                                 layer_index_name = 'nl',
                                 sep = ""){
  nl <- NULL # to pass CRAN check NOTES

  if (!is.data.table(x)) {data.table::setDT(x) }

  setnames(x, names(x), tolower(names(x)))

  if (!layer_index_name %in% names(x)) {
    stop(paste0("layer_index_name '",layer_index_name, "' not found!"))
  }
  if (is.null(layers)) {
    layers <- unique(x[[layer_index_name]])
  }

  id_cols <-c("yr","mo","da","doy","dates", layer_index_name)

  # value and id vars
  if (is.null(value_vars)) {
    value_vars <- names(x)[-which(names(x) %in% id_cols)]
  } else {
    value_vars <- match.arg(tolower(value_vars),
                            choices = names(x)[-which(names(x) %in% id_cols)],
                            several.ok = T)
  }

  id.vars <- names(x)[which(names(x) %in% id_cols[-which(id_cols == layer_index_name)])]

  setkeyv(x, layer_index_name)

  datm <- data.table::melt(x[list(layers), ], # extract layers of interest
               id.vars = c(id.vars,layer_index_name),
               measure.vars = value_vars)
  castf <- paste(paste(id.vars, collapse = "+"), "~ variable+nl")

  data.table::dcast(datm, stats::as.formula(castf), sep = sep)
}
