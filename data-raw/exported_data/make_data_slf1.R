library(data.table)
slb1_meteo <- read.csv("data-raw/exported_data/slf1nn_climate.csv", stringsAsFactors = F)
slb1_meteo$dates <- as.Date(slb1_meteo$dates, "%m/%d/%Y")
str(slb1_meteo)
setDT(slb1_meteo)
setnames(slb1_meteo, "wind", "windspeed")

slb1_meteo[, globrad := globrad*0.0864]
slb1_meteo[, vappres := sirad::es(tmax, tmin)*relhum/100]
summary(slb1_meteo)

slb1_meteo <- slb1_meteo[order(slb1_meteo$dates),]
str(meteo_slb1)
setDF(slb1_meteo)
usethis::use_data(slb1_meteo, overwrite = T)

slb1_soil <- read.csv("data-raw/exported_data/soil_slb1.csv", stringsAsFactors = F)
names(slb1_soil)[c(2,3)] <- c("upper", "lower")
setDT(slb1_soil)
slb1_soil <- slb1_soil[-c(1:3),]
setDF(slb1_soil)
usethis::use_data(slb1_soil, overwrite = T)

slb1_standprop <- read.csv("data-raw/exported_data/LongTermVegDev_slb1.csv", stringsAsFactors = F)
slb1_standprop <- slb1_standprop[,-c(1,2,4)]
names(slb1_standprop)[which(names(slb1_standprop) == "density")] <- "densef"
setDF(slb1_standprop)
usethis::use_data(slb1_standprop, overwrite = T)



# mpot_swc <- read.csv("data-raw/exported_data/slb1_swc_mpot.csv", stringsAsFactors = F)
# setDT(mpot_swc)
#
# mpot_swc[, dates := as.Date(dates, "%m/%d/%Y")]
# slb1_mpot <- mpot_swc[,list(dates, mpot_10cm, mpot_20cm, mpot_40cm, mpot_100cm, mpot_180cm)]
# slb1_swc <- mpot_swc[,list(dates, swc_20cm_a, swc_20cm_b,swc_60cm, swc_70cm)]
# setDF(slb1_mpot)
# setDF(slb1_swc)
# usethis::use_data(slb1_mpot, slb1_swc, overwrite = TRUE)
#
#' Daily values of soil water potential (1999-2009) in different soil depths
#' measured at the Solling Beech experimental site 'SLB1'.
#'
#' A dataset containing daily soil water potential in 10, 20, 40, 100, 180 cm
#' soil depths
#'
#' @format A data.frame with 4018 rows and 6 variables
#' \describe{
#'   \item{dates}{Date of measurement}
#'   \item{mpot_10cm}{soil water potential in 10cm soil depth, hPa}
#'   \item{mpot_20cm}{soil water potential in 10cm soil depth, hPa}
#'   \item{mpot_40cm}{soil water potential in 10cm soil depth, hPa}
#'   \item{mpot_100cm}{soil water potential in 10cm soil depth, hPa}
#'   \item{mpot_180cm}{soil water potential in 10cm soil depth, hPa}
#' }
"slb1_mpot"

# slb1_rootden <- read.csv("data-raw/exported_data/rootden_slb1.csv", stringsAsFactors = F)
# setDT(slb1_rootden)
# usethis::use_data(slb1_rootden, overwrite = T)
