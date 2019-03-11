#' Meteorological Data from the Solling Beech and Spruce experimental site
#'
#' A dataset containing daily weather variables for the period 1960-2013
#'
#' @format A data.table with 19724 rows and 9 variables
#' \describe{
#'   \item{dates}{date}
#'   \item{tmin}{daily minimum temperature, degC}
#'   \item{tmax}{daily maximum temperature, degC}
#'   \item{tmean}{daily mean temperature, degC}
#'   \item{prec}{daily sum of precipitation, mm}
#'   \item{relhum}{relative Humidity, %}
#'   \item{globrad}{daily sum of global radiation, MJ m-2}
#'   \item{wind}{daily mean wind speed measured at 10 m above ground, m s-2}
#'   \item{vappres}{daily vapour pressure, kPa}
#' }
"slb1_meteo"

#' Soil profile data from the Solling Beech experimental site 'SLB1'
#'
#' A dataset containing the soil horizons' physical properties
#'
#' @format A data.table with 17 rows and 10 variables
#' \describe{
#'   \item{horizon}{horizon symbol}
#'   \item{upper}{upper layer boundary, m}
#'   \item{lower}{lower layer boundary, m}
#'   \item{texture}{soil texture according to German soil texture classification system}
#'   \item{bd}{bulk density of the fine earth, g cm-3}
#'   \item{gravel}{fraction of coarse material }
#'   \item{sand}{sand content, mass-\% }
#'   \item{silt}{silt content, mass-\% }
#'   \item{clay}{clay content, mass-\% }
#'   \item{c_org}{organic carbon content, mass-\%}
#' }
"slb1_soil"

#' Annual stand properties of the Solling Beech experimental site 'SLB1'
#'
#' A dataset containing the forests's stand properties
#'
#' @format A data.table with 49 rows and 7 variables
#' \describe{
#'   \item{year}{Year of observation}
#'   \item{species}{Tree species}
#'   \item{age}{age of the main stand}
#'   \item{height}{average height of the trees, m}
#'   \item{maxlai}{maximum leaf area index, m2 m-2}
#'   \item{sai}{stem area index, m2 m-2 }
#'   \item{densef}{stand density}
#'   \item{clay}{clay content, mass-\% }
#'   \item{c_org}{organic carbon content, mass-\%}
#' }
"slb1_standprop"

