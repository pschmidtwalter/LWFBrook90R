#' Select output for LWF-Brook90
#'
#' Returns a [5,10] matrix with a default selection of LWF-Brook90 output data sets for the use
#' as 'output'-argument \code{\link{runLWFB90}}.
#'
#' @param output optional [5,10]-matrix, opened for editing.
#' If no matrix is passed, a default selection of output values is opened in R's data-editor or returned
#' instantly, if edit=FALSE.
#' @param edit open R's data-editor ?
#'
#' @return a [5,10]-matrix containing 0 and 1 for use as 'output'-argument in \code{\link{runLWFB90}}
#' @examples
#'
#' # create matrix with default selection
#'outmat <- choose_output.B90()
#'
# modify
#'outmat[6,] <- 1
#'
# open modified
#'outmat_new <- setoutput_LWFB90(outmat)
#'
# open a default output matrix in data editor
#'outmat <- setoutput_LWFB90(edit = T)
#'
#' @export
setoutput_LWFB90 <- function(outputmat = NULL,
                              edit = FALSE) {
  if (is.null(outputmat)) {
    outputmat <- matrix(ncol = 5,nrow = 10,
                        byrow = TRUE,
                        dimnames = list(c("Eval", "Budg","Flow","Evap","Abov","Belo","Swat","Psit","Misc","User"),
                                        c("Ann","Mon","Day","Pre","ITR")),
                        #     #Year, Month, Day, PInt Itr #Achtung Itr gibt keinen Output! NIcht w?hlen
                        data = c(0,    0,     0,   0,   0, #Eval
                                 0,    0,     0,   0,   0, #Budg
                                 0,    0,     0,   0,   0, #Flow
                                 1,    1,     1,   0,   0, #Evap
                                 0,    0,     0,   0,   0, #Abov
                                 0,    0,     0,   0,   0, #Belo
                                 0,    0,     1,   0,   0, #Swat
                                 0,    0,     0,   0,   0, #Psit
                                 0,    0,     0,   0,   0, #Misc
                                 0,    0,     0,   0,   0  #User
                        )
    )
    if (edit == T) {
      outputmat <- edit(outputmat)}
  } else {
    if (all(dim(outputmat) == c(10,5)) ) {
      outputmat <- edit(outputmat)
    }
    else {
      stop("Please provide a [10,5] matrix for editing!")
    }
  }
  outputmat[which(outputmat != 0)] <- 1
  outputmat
}
