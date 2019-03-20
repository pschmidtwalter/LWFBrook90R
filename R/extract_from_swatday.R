#'  Extract variables from swatday.asc data.tables and organize layer-wise values in columns
#'
#' @param dat swatday.asc data.table as returned from \code{\link{readOutput.B90}}:
#' @param layers integer vector addressing the layer numbers (nl) to be extracted
#'                   from dat. If not supplied, values from all layers will be returned
#' @param vars character vector containing variable names (out of "swati","theta",
#'                 "wetnes","psimi","psiti" to be extracted from dat. If not supplied
#'                 all 5 variables will be returned.
#'
#' @return a data.table with the layers' daily values of the variables organized in columns
#'     (wide format) with the names being made up of the variable name and layer number.
#' @export
#' @import data.table
#'
#' @examples
#' # Read files from LWF-Brook90 output directory
#'
#' output <- readOutput.B90("tests/out/")
#'
#' # Reorganize all variables for all layers
#'
#' swati_wide <- extract_from_swatday.asc(output$swatday.asc)
#'
#' # select single variables and layers:
#' swati_wide <- extract_from_swatday.asc(output$swatday.asc,
#'                                        layers =1:10,
#'                                        vars = c("swat", "psim", "theta"))
#'
extract_from_swatday.asc <- function(dat, layers = NULL, vars=NULL){
  if (!is.data.table(dat)) {setDT(dat) }
  if (is.null(layers)) { layers <- unique(dat$nl)}
  if (is.null(vars)) {
    vars <- c("swati","theta", "wetnes","psimi","psiti")
  } else {
    vars <- match.arg(vars, choices = c("swati","theta", "wetnes","psimi","psiti"), several.ok = T)
  }
  setkey(dat, nl)
  dat <- dat[list(layers), ]
  dat[, dates := as.Date(paste(yr, mo, da, sep = "-"))]
  datm <- melt(dat, id.vars = c("dates", "nl"), measure.vars = vars )
  dcast(datm, dates~variable+nl)
}
