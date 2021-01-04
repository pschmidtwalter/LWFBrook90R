#' Correct rain gauge precipitation data for wind and evaporation errors after
#' Richter (1995)
#'
#' @param month Vector of months.
#' @param tavg Vector of air temperature values (deg C). Same length as
#'   \code{month}.
#' @param prec Vector of measured rainfall vales (mm). Same length as
#'   \code{month}.
#' @param station.exposure Situation of the weather station where prec was
#'   measured: one of 'frei', 'lg', 'mg', 'sg' (corresponding to full exposure,
#'   low protected, moderate protected, strong protected situation).
#' @param full_output Logical wether to return the full data set additionally
#'   including input data, correction coefficients.
#'
#' @return A vector of corrected rainfall data, or (if \code{full_output ==
#'   TRUE}) a data.table containing the input objects, the month, the
#'   precipitation type ('N4So': liquid rain, summer; 'N4Wi' liquid rain,
#'   winter; 'N8' = sleet, 'N7' = snow), correction coefficients epsilon and b,
#'   and the corrected rainfall.
#'
#' @references Richter, D. (1995)
#' Ergebnisse methodischer Untersuchungen zur Korrektur des systematischen
#' Messfehlers des Hellmann-Niederschlagsmessers.
#' \emph{Berichte des Deutschen Wetterdienstes}, \bold{194}, 93 pp, Offenbach,
#' Germany
#'
#' @export
#'
#' @examples
#' clim <- slb1_meteo[as.integer(format(slb1_meteo$dates,"%Y")) %in% 2001:2005,]
#' clim$month <- as.integer(format(clim$dates, "%m"))
#'
#' prec_meas <- clim$prec
#' prec_corr_frei <- with(clim,
#'                        prec_corr(month, tmean, prec, station.exposure = "frei"))
#' prec_corr_lg <- with(clim,
#'                      prec_corr(month, tmean, prec, station.exposure = "lg"))
#' prec_corr_mg <- with(clim,
#'                      prec_corr(month, tmean, prec, station.exposure = "mg"))
#' prec_corr_sg <- with(clim,
#'                      prec_corr(month, tmean, prec, station.exposure = "sg"))
#'
#' plot(clim$dates, cumsum(prec_corr_frei),
#' type = "l", col = "violet", xlab = "dates", ylab = "cum. precipitation (mm)")
#' lines(clim$dates, cumsum(prec_corr_lg), col = "blue")
#' lines(clim$dates, cumsum(prec_corr_mg), col = "green")
#' lines(clim$dates, cumsum(prec_corr_sg), col = "red")
#' lines(clim$dates, cumsum(prec_meas))
#' legend('bottomright', c('frei', "lg", "mg", "sg"),
#'        col = c("violet", "blue", "green", "red", "black"),
#'        lty = 1, pch = NULL )
prec_corr <-  function(month,tavg,prec,
                       station.exposure = "mg",
                       full_output = FALSE
){

  if (length(month) != length(tavg))
    stop("data vectors have to be of the same lengths")
  if (length(month) != length(prec))
    stop("data vectors have to be of the same lengths")

  station.exposure <- match.arg(station.exposure, choices = c("frei","lg","mg","sg"))

  #----------------------------------------------------------------------------

  dat <- data.frame(month,tavg,prec)

  # epsilon
  eps <- c(N4So=0.38, N4Wi=0.46, N8=0.55,N7=0.82)

  # b coefficents
  bcoeff <- matrix(c(0.345,0.34,0.535,0.72,  0.31,0.28,0.39,0.51,  0.28,0.24,0.305,0.33,  0.245,0.19, 0.185,0.21),
                   nrow = 4,
                   byrow = FALSE,
                   dimnames = list(c("N4So","N4Wi","N8","N7"),
                                  c("frei","lg","mg","sg")))

  #precipitation types
  dat$prectype <- ifelse(tavg >= 3,
                           ifelse(month %in% 5:10,  #liquid
                                  "N4So", "N4Wi"), #summer/winter
                           ifelse(tavg < 0.7, #not liquid
                                  "N7", "N8")) #snow / sleet

  # Niederschlag korrigieren
  dat$b <- bcoeff[dat$prectype, station.exposure]
  dat$eps <- utils::stack(eps[dat$prectype])$values
  dat$preccorr <- prec + dat$b * prec**dat$eps

  #output
  if (full_output == FALSE) {
    return(dat$preccorr)
  } else return(dat)
}
