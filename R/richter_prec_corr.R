#' Correct rain gauge precipitation data for wind and evaporation errors after Richter (1995)
#'
#' @param dates a vector of Dates
#' @param tavg a vector of air temperature
#' @param prec a vector of rainfall
#' @param station.exposure situation of the weather station where prec was measured: one of 'frei', 'lg', 'mg', 'sg'
#' (corresponding to full exposure, low protected, moderate protected, strong protected situation)
#' @param full.output logical wether to return the full data set additionally including input data, correction coefficients.
#'
#' @return a vector of corrected rainfall data, or (if full.output == TRUE) a data.table containing the input objects, the month,
#' the precipitation type ('N4So': liquid rain, summer; 'N4Wi' liquid rain, winter; 'N8' = sleet, 'N7' = snow),
#' correction coefficients epsilon and b, and the corrected rainfall.
#'
#' @references
#' Richter, D. (1995): "Ergebnisse methodischer Untersuchungen
# zur Korrektur des systematischen Messfehlers des Hellmann-Niederschlagsmessers."
#' \emph{Berichte des Deutschen Wetterdienstes}, \bold{194}, 93 pp, Offenbach, Germany
#'
#' @export
#'
#' @examples
#'
#' prec_corr(slb1_meteo$dates, slb1_meteo$tmean, slb1_meteo$prec)
#' prec <- with(slb1_meteo[year(dates) %in% 2001:2010,],
#'              data.frame(dates = dates, meas = prec,
#'                         frei = prec_corr(dates, tmean, prec, station.exposure = "frei"),
#'                         lg = prec_corr(dates, tmean, prec, station.exposure = "lg"),
#'                         mg = prec_corr(dates, tmean, prec, station.exposure = "mg"),
#'                         sg = prec_corr(dates, tmean, prec, station.exposure = "sg"))
#' )
#'
#'
#' plot(prec$dates, cumsum(prec$frei), type = "l", col = "violet", xlab = "dates", ylab = "cum. precipitation (mm)")
#' lines(prec$dates, cumsum(prec$lg), col = "blue")
#' lines(prec$dates, cumsum(prec$mg), col = "green")
#' lines(prec$dates, cumsum(prec$sg), col = "red")
#' lines(prec$dates, cumsum(prec$meas))
#' legend('bottomright', c('frei', "lg", "mg", "sg"), col = c("violet", "blue", "green", "red", "black"), lty = 1, pch = NULL )

prec_corr <-  function(dates,tavg,prec,
                       station.exposure = "mg",
                       full.output = FALSE
){

  if (missing(dates) || missing(tavg) || missing(prec))
    stop("dates, tavg, prec must be given!")
  if (length(dates) != length(tavg))
    stop("data vectors have to be of the same lengths")
  if (length(dates) != length(prec))
    stop("data vectors have to be of the same lengths")
  if (class(dates) != "Date") {
    stop("Please provide dates as Date object!")
  }

  station.exposure <- match.arg(station.exposure, choices = c("frei","lg","mg","sg"))

  #----------------------------------------------------------------------------

  dat <- data.table(dates,tavg,prec)
  dat[, month := month(dates)]

  # epsilon
  eps <- c(N4So=0.38, N4Wi=0.46, N8=0.55,N7=0.82)

  # b coefficents
  bcoeff <- matrix(c(0.345,0.34,0.535,0.72,  0.31,0.28,0.39,0.51,  0.28,0.24,0.305,0.33,  0.245,0.19, 0.185,0.21),
                   nrow = 4,
                   byrow = FALSE,
                   dimnames = list(c("N4So","N4Wi","N8","N7"),
                                  c("frei","lg","mg","sg")))

  #precipitation types
  dat[, prectype := ifelse(tavg >= 3,
                           ifelse(month %in% 5:10,  #liquid
                                  "N4So", "N4Wi"), #summer/winter
                           ifelse(tavg < 0.7, #not liquid
                                  "N7", "N8")) #snow / sleet
      ]

  # Niederschlag korrigieren
  dat[, b := bcoeff[prectype, station.exposure]]
  dat[, eps := stack(eps[prectype])$values]
  dat[, preccorr := prec + b * prec**eps]

  #output
  if (full.output == FALSE) {
    return(dat$preccorr)
  } else return(dat)
}
