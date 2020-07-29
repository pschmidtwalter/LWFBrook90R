#'  Mualem - van Genuchten Parameters from regression functions developed by Puhlmann & von Wilpert 2011
#'
#' Derives the Mualem - van Genuchten parameters ThetaR, ThetaS, Alpha, n, Tort and Ksat
#'     from clay, silt, sand contents, bulk density, and organic carbon content values
#'
#' @param clay Numeric vector of clay mass percent
#' @param silt Numeric vector of silt mass percent.
#' @param sand Numeric vector of sand mass percent.
#' @param bd Numeric vector of bulk density in g cm-3.
#' @param oc.pct Numeric vector of organic carbon content in mass percent.
#'
#' @return A data.frame with the number of rows equal to length(tex.KA5).
#' The data.frame contains the following variables:
#' \describe{
#'   \item{ths}{Saturation water content fraction}
#'   \item{thr}{Residual water content fraction}
#'   \item{npar}{N parameter of van Genuchten water retention function}
#'   \item{mpar}{M parameter of van Genuchten water retention function}
#'   \item{alpha}{Alpha parameter of van Genuchten water retention function, m-1}
#'   \item{ksat}{Saturated hyraulic conductivity parameter of Mualem hydraulic conductivity function, mm d-1}
#'   \item{tort}{Tortuosity parameter of Mualem hydraulic conductivity function}
#' }
#' @references Puhlmann H, von Wilpert K (2011) Testing and development of pedotransfer
#'                 functions for water retention and hydraulic conductivity of forest soils.
#'                 Wald?kologie, Landschaftsforschung und Naturschutz 12, pp. 61-71
#' @examples
#' hydpar_puh2(20,20,60,1.5)
#' @export
#'
hydpar_puh2 <- function(clay, silt, sand, bd, oc.pct=0.5){
  if (is.null(clay) || is.null(silt) || is.null(sand) || is.null(bd)) {
    stop("Please provide sand, silt, clay contents and bulk density")
  }
  if (length(unique(lengths(list(clay, silt, sand, bd))))>1) {
    stop("Sand, silt, clay, bulk density must have equal lengths")
  }
  out <- data.frame( clay, silt, sand, bd, oc.pct, stringsAsFactors=F)
  out$id= 1:nrow(out)
  #out[which( !(out$bodenart %in% wess_nfk$Texture.KA5)), c("clay", "silt", "sand", "bd", "ocpct") ] <- NA # Sicherheit dass f?r Torfe nichts berechnet wird


  out <- within(out,{
    #MvG
    ths <- 0.015362*(oc.pct^0.5) - 0.2513*bd - 0.026836*log(clay+1) - 0.0055404*(sand^0.5) + 0.8648
    thr <- 0.069 #konstant
    alpha  <- exp( -1.187*(bd^2) - 0.031899*sand - 0.58805*log(oc.pct+0.1) - 0.00032963*(silt^2) - 0.016267*silt*bd + 2.021 )
    npar <- exp( 0.0003758*(sand^2) + 0.004751*silt + 0.017826*(silt/bd)  -2.9804) +1
    mpar <- 1-1/npar
    ksat <- 10^( -1.2491*(bd^2) - 0.00087388*(clay^2) - 1.10316 ) *10*86400   # mm/d
    #KSat <-  -1.2491*(bd^2) - 0.00087388*(clay^2) - 1.10316
    tort <- -0.98063*log(sand+0.1) - 0.004075*(clay^2) + 0.030022*clay*oc.pct -0.00457*(sand/oc.pct) +4.4304

    #point estimates
    # PV <- (-0.38372*bd + 1.0082)*100
    # FC <- (-0.000020764*(sand^2) + 0.042095*log(oc.pct+0.5) + 0.00025376*silt*bd + 0.343)*100 # zu gering, manchmal sogar negativ!
    # AWC <- (-0.00002145*(sand^2) - 0.00009023*(clay^2) + 0.026072*log(oc.pct+0.1) + 0.2537)*100
    # AC <- ( -0.38372*bd +1.0082 )*100 -FC #Porosit?t - FK
    # PWP <- FC-AWC
  })
  out$alpha <- out$alpha*100
  return(out[order(out$id),c("ths", "thr", "alpha", "npar","mpar", "ksat", "tort")])
}
