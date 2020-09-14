#' Mualem - van Genuchten Parameters from regression functions developed by Woesten et al 1999
#'
#' Derives the Mualem - van Genuchten parameters ThetaR, ThetaS, Alpha, n, Tort and Ksat
#' from sand, silt, clay, bulk density, organic carbon content values.
#'
#' @param clay Numeric vector of clay mass percent.
#' @param silt Numeric vector of silt mass percent.
#' @param bd Numeric vector of bulk density in g cm-3.
#' @param oc.pct Numeric vector of organic carbon content in mass percent.
#' @param topsoil Logical: Is the sample from the topsoil?
#' @param humconv Conversion factor oc.pct to organic matter percent.
#'
#' @return A data.frame with the number of rows equal to length(clay)
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
#' @references Woesten JHM, Lilly A, Nemes A, Le Bas C  (1999) Development and use of
#'                 a database of hydraulic properties of European soils. Geoderma 90, pp. 169-185
#' @export
#'
#' @examples
#' hydpar_hypres(20,20,1.5,2)
hydpar_hypres <- function(clay, silt, bd, oc.pct=0.1, topsoil=TRUE, humconv=1.72 ){
  h <- NULL #pass CRAN check Notes

  out <- data.frame(clay=clay/100,silt=silt/100,bd=bd*1000,
                    h=ifelse(oc.pct < 0.1,0.1,oc.pct/100), topsoil,
                    stringsAsFactors=F)


  #constrains
  out$h <- ifelse( (out$clay >0.6 & out$h>0.18) ,0.18, out$h )
  out$h <- ifelse( (out$clay <=0.6 & out$h > (0.12+ 0.1 * out$clay) ), (0.12 + 0.1 * out$clay), out$h)
  out$h <- out$h*humconv #humus conversion

  out$bd <- ifelse(out$bd<500, 500, out$bd)
  out$clay <- ifelse(out$clay <0.005, 0.005, out$clay)
  out$silt <- ifelse(out$silt <0.005, 0.005, out$silt)

  out <- within(out,{

    thr <- 0
    ths <-  (0.7919 + 0.1691 * clay - 0.00029619 * bd - 0.01491 * silt * silt +
    0.821 * h * h + 0.0002427 / clay + 0.0001113 / silt +
    0.01472 * log(silt * 100) - 0.733 * h * clay - 0.0000619 * bd * clay -
    0.0001183 * bd * h - 0.01664 * topsoil * silt)

    alpha <- exp(-14.96 + 3.135 * clay + 3.51 * silt + 64.6 * h +
                   0.01529 * bd - 0.192 * topsoil - 0.000004671 * bd * bd - 7.81 * clay * clay -
                   68.7 * h * h + 0.000449 / h + 0.0663 * log(100 * silt) +
                   0.1482 * log(100 * h) - 0.004546 * bd * silt - 0.04852 * bd * h +
                   0.673 * topsoil * clay )

    npar <- exp(-25.23 - 2.195 * clay + 0.74 * silt - 19.4 * h +
                   0.0455 * bd - 0.00000724 * bd * bd + 3.658 * clay * clay +
                   28.85 * h * h - 12810 / bd - 0.001524 / silt -
                   0.0001958 / h - 0.2876 * log(silt * 100) - 0.0709 * log(100 * h) -
                   44.6 * log(bd / 1000) - 0.002264 * bd * clay + 0.00896 * bd * h +
                   0.718 * topsoil * clay) + 1
    ksat <- exp(7.755 + 3.52 * silt + 0.93 * topsoil - 0.000000967 * bd * bd - 4.84 * clay * clay -
                  3.22 * silt * silt + 0.001 / (silt * 100) - 0.0748 / (h * 100) -
                  0.643 * log(silt * 100) - 0.001398 * bd * clay - 0.01673 * bd * h +
                  2.986 * topsoil * clay - 3.305 * topsoil * silt) * 10 # conversion to mm/d

    tort <- 0.0202 + 6.193 * clay * clay - 11.36 * h * h - 0.2316 * log(h * 100) - 0.003544 * bd * clay +
      0.000283 * bd * silt + 0.00488 * bd * h
  })
  out$tort <-  10 * (exp(out$tort) - 1) / (exp(out$tort) + 1)
  out$mpar <- 1-1/out$n
  out[,c(1,2)] <- out[,c(1,2)]*100
  out$alpha <- out$alpha*100
  out[,c("ths","thr", "alpha","npar","mpar","ksat","tort")]
}
