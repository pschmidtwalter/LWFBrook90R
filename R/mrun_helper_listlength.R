#' Determine list length of nested lists
#'
#' @param ll a list
#'
#' @return integer value with the length at uppermost level. Flat lists have length of 1.
#' @export
nstlist_length <- function(ll) {
  if (any(sapply(ll, is.list))) {
    len <- length(ll)
  } else {
    len <- 1L
  }
  return(len)
}
