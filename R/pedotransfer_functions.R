#' Functions to derive soil hydraulic properties from soil physical properties
#'
#' A set of pedotransfer functions for deriving Mualem - van Genuchten
#' parameters from soil physical properties of soil horizons, such as soil
#' texture, bulk density and carbon content.
#'
#' @param clay,silt,sand Numeric vectors of clay, silt, sand in mass \%.
#'   Particle size ranges for clay, silt and sand correspond to <2, 2-63, and
#'   63-2000 \eqn{\mu m}{\mum}. For \code{\link{hydpar_hypres}}, the particle
#'   size limit between silt and sand should be 50 \eqn{\mu m}{\mum}.
#' @param bd Numeric vector of bulk density in g cm-3.
#' @param oc.pct Numeric vector of organic carbon content in mass \%.
#' @param texture Character vector of soil texture classes. For
#'   \code{hydpar_wessolek_tab} classes according to KA5 (AG Boden 2005) have to
#'   be provided. When using \code{\link{hydpar_hypres_tab}}, texture classes
#'   according to FAO (1990) have to provided.
#' @param topsoil Logical vector stating if a sample is from the topsoil or the
#'   subsoil. Used in \code{hydpar_hypres_tab} and \code{hydpar_hypres}.
#' @param humconv Conversion factor from oc.pct to organic matter percent.
#'   Default: 1.72. Only for \code{hydpar_hypres_tab} and \code{hydpar_hypres}.
#' @param n An integer value specifying the number of rows of the returned
#'   data.frame (i.e. the number of repetitions of the MvG-Parameter set, only
#'   for \code{hydpar_ff_hamken}).
#'
#' @return A data.frame with the following variables: \describe{
#'   \item{ths}{Saturation water content fraction} \item{thr}{Residual water
#'   content fraction} \item{npar}{N parameter of the van Genuchten water
#'   retention function} \item{mpar}{M parameter of the van Genuchten water
#'   retention function} \item{alpha}{Alpha parameter of the van Genuchten water
#'   retention function (1/m)} \item{ksat}{Saturated hyraulic conductivity
#'   parameter of Mualem hydraulic conductivity function (mm/d)}
#'   \item{tort}{Tortuosity parameter of Mualem hydraulic conductivity function}
#'   }
#'
#' @details
#' Function \code{hydpar_puh2} derives Mualem - van Genuchten (MvG) parameters
#' using the regression functions developed by Puhlmann & von Wilpert (2011).
#' The equations of Wösten et al. (1999) are available via \code{hydpar_hypres},
#' and their tabulated values for soil texture classes can be derived using the
#' function \code{hydpar_hypres_tab}. The table of MvG parameters from Wesselok
#' et al. (2009; Tab. 10) is accessible by \code{hydpar_wessolek_tab}. For this
#' function, soil texture classes after the German texture classification system
#' (KA5, AG Boden 2005) have to be provided. To derive hydraulic parameters of
#' forest floor horizons, the function \code{hydpar_ff_b90} can be used. It
#' returns the single MvG parameter set for forest floor horizons reported by
#' Hammel & Kennel (2001) in their original LWF-Brook90 publication.
#'
#' @references
#'
#' AG Boden (2005)
#' Bodenkundliche Kartieranleitung
#' Schweizerbart'sche Verlagsbuchhandlung, Stuttgart
#'
#' Food and Agriculture Organisation (FAO) (1990)
#' Guidelines for soil description
#' FAO/ISRIC, Rome, 3rd edition
#'
#' Hammel K & Kennel M (2001)
#' Charakterisierung und Analyse der Wasserverfügbarkeit und des Wasserhaushalts
#' von Waldstandorten in Bayern mit dem Simulationsmodell BROOK90.
#'\emph{Forstliche Forschungsberichte München} 185
#'
#' Puhlmann H, von Wilpert K (2011)
#' Testing and development of pedotransfer functions for water retention and
#' hydraulic conductivity of forest soils.
#' \emph{Waldökologie, Landschaftsforschung und Naturschutz} 12, pp. 61-71
#'
#' Wessolek G, Kaupenjohann M and Renger H (2009)
#' Bodenphysikalische Kennwerte und Berechnungsverfahren für die Praxis.
#' \emph{Bodenökologie und Bodengenese} 40, Berlin, Germany
#'
#' Woesten JHM, Lilly A, Nemes A, Le Bas C  (1999)
#' Development and use of a database of hydraulic properties of European soils.
#' \emph{Geoderma} 90, pp. 169-185
#'
#' @example inst/examples/pedotransfer_functions-help.R
#' @name ptfs
NULL

#' @rdname ptfs
#' @export
hydpar_puh2 <- function(clay, silt, sand, bd, oc.pct=0.5){
  if (is.null(clay) || is.null(silt) || is.null(sand) || is.null(bd)) {
    stop("Please provide sand, silt, clay contents and bulk density")
  }
  if (length(unique(lengths(list(clay, silt, sand, bd))))>1) {
    stop("Sand, silt, clay, bulk density must have equal lengths")
  }
  out <- data.frame( clay, silt, sand, bd, oc.pct, stringsAsFactors=FALSE)
  out$id= 1:nrow(out)
  #out[which( !(out$bodenart %in% wess_nfk$Texture.KA5)), c("clay", "silt", "sand", "bd", "ocpct") ] <- NA # Sicherheit dass f?r Torfe nichts berechnet wird


  out <- within(out,{
    #MvG
    ths <- 0.015362*(oc.pct^0.5) - 0.2513*bd - 0.026836*log(clay+1) - 0.0055404*(sand^0.5) + 0.8648
    thr <- 0.069
    alpha  <- exp( -1.187*(bd^2) - 0.031899*sand - 0.58805*log(oc.pct+0.1) - 0.00032963*(silt^2) - 0.016267*silt*bd + 2.021 )
    npar <- exp( 0.0003758*(sand^2) + 0.004751*silt + 0.017826*(silt/bd)  -2.9804) +1
    mpar <- 1-1/npar
    ksat <- 10^( -1.2491*(bd^2) - 0.00087388*(clay^2) - 1.10316 ) *10*86400   # mm/d
    tort <- -0.98063*log(sand+0.1) - 0.004075*(clay^2) + 0.030022*clay*oc.pct -0.00457*(sand/oc.pct) +4.4304

  })
  out$alpha <- out$alpha*100
  return(out[order(out$id),c("ths", "thr", "alpha", "npar","mpar", "ksat", "tort")])
}

#' @rdname ptfs
#' @export
hydpar_hypres <- function(clay, silt, bd, oc.pct, topsoil, humconv=1.72 ){
  h <- NULL #pass CRAN check Notes

  out <- data.frame(clay=clay/100,silt=silt/100,bd=bd*1000,
                    h=ifelse(oc.pct < 0.1,0.001,oc.pct/100), topsoil,
                    stringsAsFactors=FALSE)


  #constrain OC (FAO definition for histic horizons)
  out$h <- ifelse( (out$clay >0.6 & out$h>0.18) ,0.18, out$h )
  out$h <- ifelse( (out$clay <=0.6 & out$h > (0.12+ 0.1 * out$clay) ), (0.12 + 0.1 * out$clay), out$h)
  out$h <- out$h*humconv # conversion from organic carbon to organic matter

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

    tort <- 0.0202 + 6.193 * clay * clay - 11.36 * h * h - 0.2316 * log(h * 100) -
      0.003544 * bd * clay +0.000283 * bd * silt + 0.00488 * bd * h
  })
  out$tort <-  10 * (exp(out$tort) - 1) / (exp(out$tort) + 1)
  out$mpar <- 1-1/out$n
  out[,c(1,2)] <- out[,c(1,2)]*100
  out$alpha <- out$alpha*100
  out[,c("ths","thr", "alpha","npar","mpar","ksat","tort")]
}

#' @rdname ptfs
#' @export
hydpar_hypres_tab <- function(texture, topsoil){
  if (is.null(texture) || is.null(topsoil)){
    stop("Please provide soil texture according to FAO definition (C, M, MF, F, VF or Org)
         and state if the sample was taken in the topsoil or not.")
  }
  if (length(texture) != length(topsoil)) {
    stop("soil texture and bulk density must have equal lengths!")
  }
  topsoil <- as.logical(topsoil)

  out <- data.frame(id = 1:length(texture),texture, topsoil,
                    stringsAsFactors = FALSE)
  out <- merge(out, hypres_tab4, by=c("texture","topsoil" ), all.x = TRUE )
  out$alpha <- out$alpha*100
  out[order(out$id), c("ths", "thr", "alpha","npar","mpar","ksat","tort")]
}

#' @rdname ptfs
#' @export
hydpar_wessolek_tab <- function(texture) {
  if (is.null(texture)) {
    stop("Please provide the soil texture according to KA5" )
  }
  out <- data.frame(id=seq(1,length(texture)), texture)
  out <- merge(out, wessolek_mvg_tab10[,c("texture","ths","thr","alpha","npar","mpar","ksat","tort")],
               by="texture", all.x = TRUE)
  out$alpha <- out$alpha*100
  out[order(out$id), -which(names(out) %in% c("texture","id"))]
}

#' @rdname ptfs
#' @export
hydpar_ff_b90 <- function(n=1) {
  if (length(n) > 1) {
    warning("Only the first element of the supplied vector will be used." )
  }
  out <- hydpar_forestfloor[rep(1,n[1]),]
  out
}







