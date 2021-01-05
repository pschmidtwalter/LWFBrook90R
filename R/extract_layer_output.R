#' Extracts values from layer data and organizes layer-wise variables in columns
#'
#' Convenience function to reorganize soil layer data such as BELO and SWAT to
#' the wide format, by casting variables with the layer number, using
#' data.table's \code{dcast}-function.
#'
#' @param dat Data.frame with layer data organized in rows and identified by a
#'   layer number column \code{nl}.
#' @param layers Integer vector addressing the layer numbers (nl) to be
#'   extracted from dat. If not supplied, values from all layers will be
#'   returned.
#' @param value_vars Character vector containing names of value-variables to be
#'   extracted from dat. If not supplied, \code{value_vars} will be guessed.
#' @param sep Separation character for constructig names from variable name and
#'   layer index.
#'
#' @return A data.table with the layers' values of the variables organized in
#'   columns (wide format) with the names being made up of the variable name and
#'   layer number.
#' @export
#' @example inst/examples/extract_layer_output-help.R
#' @import data.table
extract_layer_output <- function(dat,
                                 layers = NULL,
                                 value_vars = NULL,
                                 sep = ""){
  nl <- NULL # to pass CRAN check NOTES

  if (!is.data.table(dat)) {data.table::setDT(dat) }

  setnames(dat, names(dat), tolower(names(dat)))

  if (!"nl" %in% names(dat)) {
    stop("No layer data. Missing column name 'nl' (layer-number 1:nrow(soil_nodes))")
  }
  if (is.null(layers)) {
    layers <- unique(dat$nl)
  }

  # value and id vars
  if (is.null(value_vars)) {
    value_vars <- names(dat)[-which(names(dat) %in% c("yr","mo","da","doy", "nl"))]
  } else {
    value_vars <- match.arg(tolower(value_vars), choices = names(dat)[-which(names(dat) %in% c("yr","mo","da","doy", "nl"))], several.ok = T)
  }

  id.vars <- names(dat)[which(names(dat) %in% c("yr","mo","da","doy"))]

  setkey(dat, nl)

  datm <- data.table::melt(dat[list(layers), ], # extract layers of interest
               id.vars = c(id.vars,"nl"),
               measure.vars = value_vars)
  castf <- paste(paste(id.vars, collapse = "+"), "~ variable+nl")
  data.table::dcast(datm, stats::as.formula(castf), sep = sep)
}
