#' Mualem - van Genuchten Parameters from W?sten et al. 1999, Tab. 4
#'
#' Derives the MVG Parameters ThetaR, ThetaS, Alpha, n, Tort and Ksat
#'     from soil texture classes and bulk density values
#'
#' @param tex.hypres A character vector with soil texture values according to classes applied in W?sten et al. 2009
#' @param topsoil logical Is the sample from the topsoil?
#'
#' @return A data.frame with the number of rows equal to length(tex.hypres)
#' The data.frame contains the following variables:
#' \describe{
#'   \item{ths}{Saturation water content fraction}
#'   \item{thr}{Residual water content fraction}
#'   \item{alpha}{Alpha parameter of van Genuchten water retention function, m-1}
#'   \item{npar}{N parameter of van Genuchten water retention function}
#'   \item{mpar}{M parameter of van Genuchten water retention function}
#'   \item{ksat}{Saturated hyraulic conductivity parameter of Mualem hydraulic conductivity function, mm d-1}
#'   \item{tort}{Tortuosity parameter of Mualem hydraulic conductivity function}
#' }
#' @references W?sten JHM, Lilly A, Nemes A, Le Bas C  (1999) Development and use of
#'                 a database of hydraulic properties of European soils. Geoderma 90, pp. 169-185
#'
#' @export
#'
#' @examples
#'
hydpar_hypres_tab <- function(tex.hypres, topsoil){
  if (is.null(tex.hypres) || is.null(topsoil)){
    stop("Please provide soil texture according to hypres (C, M, MF, F, VF or Org)
         and if the sample was taken in the topsoil or not.")
  }
  if (length(tex.hypres) != length(topsoil)) {
    stop("soil texture and bulk density must have equal lengths!")
  }
  topsoil <- as.logical(topsoil)

  out <- data.frame(id = 1:length(tex.hypres),tex.hypres, topsoil, stringsAsFactors = F)
  out <- merge(out, LWFBrook90R:::hypres_tab4, by=c("tex.hypres","topsoil" ), all.x = T )
  out$alpha <- out$alpha*100
  out[order(out$id), c("ths", "thr", "alpha","npar","mpar","ksat","tort")]
}
