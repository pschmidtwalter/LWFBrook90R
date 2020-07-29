#' Mualem - van Genuchten Parameters from Wessolek et al. 2009, Table 10.
#'
#' Derives the MVG Parameters ThetaR, ThetaS, Alpha, n, Tort and Ksat
#' from KA5 soil texture classes
#'
#' @param tex.KA5 A character vector with soil texture values according to KA5.
#'
#' @return A data.frame with the number of rows equal to length(tex.KA5).
#' The data.frame contains the following variables:
#' \describe{
#'   \item{tex.KA5}{soil texture according to KA5 }
#'   \item{thr}{Residual water content fraction}
#'   \item{ths}{Saturation water content fraction}
#'   \item{alpha}{Alpha parameter of van Genuchten water retention function, m-1}
#'   \item{npar}{N parameter of van Genuchten water retention parameter}
#'   \item{mpar}{M parameter of van Genuchten water retention parameter}
#'   \item{tort}{Tortuosity parameter of Mualem hydraulic conductivity function}
#'   \item{ksat}{Saturated hyraulic conductivity parameter of Mualem hydraulic conductivity function, mm d-1}

#' }
#' @references Wessolek, Kaupenjohann and Renger (2009) Bodenphysikalische Kennwerte
#'                 und Berechnungsverfahren f?r die Praxis. Boden?kologie und Bodengenese 40,
#'                 Berlin, Germany (Table No. 10)
#' @export
hydpar_wessolek_mvg <- function(tex.KA5) {
  if (is.null(tex.KA5)) {
    stop("Please provide the soil texture according to KA5" )
  }
  out <- data.frame(id=seq(1,length(tex.KA5)), tex.KA5)
  out <- merge(out, wessolek_mvg_tab10[,c("tex.KA5","ths","thr","alpha","npar","mpar","ksat","tort")], by="tex.KA5", all.x = TRUE)
  out$alpha <- out$alpha*100
  out[order(out$id), -which(names(out) %in% c("tex.KA5","id"))]
}
