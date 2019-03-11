#' Mualem - van Genuchten Parameters for forest floor organic soil horizons, from Hammel & Kennel 2001
#'
#' Derives a set of values for MVG Parameters ThetaR, ThetaS, Alpha, n, Tort and Ksat
#' for forest floor horizons as decribed in Hammel & Kennel 2001.
#'
#' @param n An integer value specifying the number of rows the returned data.frame should have
#' (i.e. the number of repetitions of the MvG-Parameter set)
#'
#' @return A data.frame with the number of rows equal to length(tex.KA5)
#' The data.frame contains the following variables:
#' \describe{
#'   \item{ths}{Saturation water content fraction}
#'   \item{thr}{Residual water content fraction}
#'   \item{alpha}{Alpha parameter of van Genuchten water retention function, m-1 }
#'   \item{npar}{N parameter of van Genuchten water retention function}
#'   \item{mpar}{M parameter of van Genuchten water retention function}
#'   \item{ksat}{Saturated hyraulic conductivity parameter of Mualem hydraulic conductivity function, mm d-1}
#'   \item{tort}{Tortuosity parameter of Mualem hydraulic conductivity function}
#' }
#' @references Hammel & Kennel (2001) Charakterisierung und Analyse der Wasserverfügbarkeit
#'                 und des Wasserhaushalts von Waldstandorten in Bayern mit dem Simulationsmodell BROOK90,
#'                 Forstliche Forschungsberichte München 185
#' @export
hydpar_forestfloor_hamken <- function(n=1) {
  if (length(n) > 1) {
    warning("Only the first element of the supplied vector will be used." )
  }
  out <- brook90r:::hydpar_forestfloor[rep(1,n[1]),]
  out$alpha <- out$alpha*100
  out
}

