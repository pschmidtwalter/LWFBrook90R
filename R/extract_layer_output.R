#'  Extract values from layer data and organize layer-wise variables in columns
#'
#'  Reorganizes layer data such as BELO and SWAT to the wide format
#'  by casting variables with the layer number, using data.table's dcast-function.
#'
#' @param dat Data.frame with layer data organized in rows and identified by a layer number column nl.
#' @param layers Integer vector addressing the layer numbers (nl) to be extracted
#' from dat. If not supplied, values from all layers will be returned.
#' @param value.vars Character vector containing names of value-variables to be extracted from dat.
#' If not supplied, value.var will be guessed.
#' @param sep Separation character for constructig names from variable name and layer index.
#'
#' @return A data.table with the layers' values of the variables organized in columns
#' (wide format) with the names being made up of the variable name and layer number.
#' @export
#' @examples
#' # create a data.frame with monthly values
#' # identifiers: layer number, yr and mo
#' df <- expand.grid(nl = 1:5,
#'                   yr = 2002:2003,
#'                   mo = 1:12)
#' #value.var
#' df$var <- runif(nrow(df), -1,0)
#'
#' extract_layer_output(df)
#'
#' #more variables
#' df$var1 <- runif(nrow(df), 1,2)
#' df$var2 <- runif(nrow(df), 2,3)
#' # extract specific layers
#' extract_layer_output(df,layers = 2:4, sep = "_")
#' #extract specific variables
#' extract_layer_output(df, layers = 2:4, value.vars = c("var1", "var2"), sep = "_")
#' @import data.table
extract_layer_output <- function(dat,
                                 layers = NULL,
                                 value.vars=NULL,
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
  if (is.null(value.vars)) {
    value.vars <- names(dat)[-which(names(dat) %in% c("yr","mo","da","doy", "nl"))]
  } else {
    value.vars <- match.arg(tolower(value.vars), choices = names(dat)[-which(names(dat) %in% c("yr","mo","da","doy", "nl"))], several.ok = T)
  }

  id.vars <- names(dat)[which(names(dat) %in% c("yr","mo","da","doy"))]

  setkey(dat, nl)

  datm <- data.table::melt(dat[list(layers), ], # extract layers of interest
               id.vars = c(id.vars,"nl"),
               measure.vars = value.vars)
  castf <- paste(paste(id.vars, collapse = "+"), "~ variable+nl")
  data.table::dcast(datm, stats::as.formula(castf), sep = sep)
}
