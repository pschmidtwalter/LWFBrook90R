#' Select output for LWF-Brook90
#'
#' Returns a \code{[7,5]} matrix with a default selection of LWF-Brook90 output
#' data sets for the use as 'output'-argument \code{\link{run_LWFB90}}.
#'
#' @param output optional \code{[7,5]}-matrix, which is opened on R's
#'   data-editor if \code{edit = TRUE}. If no matrix is passed, a default
#'   selection of output values is returned opened in R's data-editor.
#' @param edit open R's data-editor ?
#'
#' @return a \code{[7,5]}-matrix containing \code{0} and \code{1} for use as
#'   \code{output}-argument in \code{\link{run_LWFB90}}
#'
#' @examples
#' # create matrix with default selection
#' output <- set_outputLWFB90()
#' output
#'
#' # modify
#' output[,] <- 0L
#' output[,3] <- 1L
#' output["Evap", c("Ann","Mon")] <- 1L
#' output
#' @export
set_outputLWFB90 <- function(output = NULL,
                             edit = FALSE) {
  if (is.null(output)) {
    output <- matrix(ncol = 5,nrow = 7,
                     byrow = TRUE,
                     dimnames = list(c("Budg","Flow","Evap","Abov","Belo","Swat","Misc"),
                                     c("Ann","Mon","Day","Pre", "ITR")),
                     #     #Year, Month, Day, PInt Itr  # Itr does not produce output
                     data = c(0,    0,     0,   0,   0, #Budg
                              0,    0,     0,   0,   0, #Flow
                              1,    1,     1,   0,   0, #Evap
                              0,    0,     0,   0,   0, #Abov
                              0,    0,     0,   0,   0, #Belo
                              0,    0,     1,   0,   0, #Swat
                              0,    0,     0,   0,   0  #Misc
                     ))

    if (edit == TRUE) {
      output <- edit(output)}
  } else {
    if (all(dim(output) == c(7,5)) ) {
      output <- edit(output)
    }
    else {
      stop("Please provide a [7,5] matrix for editing!")
    }
  }
  output[which(output != 0)] <- 1
  output
}
