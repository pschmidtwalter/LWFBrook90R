library(LWFBrook90R)
library(data.table)
data("slb1_soil")
data("slb1_meteo")
options.b90 <- setoptions_LWFB90(startdate = as.Date("2002-06-01"), enddate = as.Date("2002-06-02"))
param.b90 <- setparam_LWFB90()
soil <- cbind(slb1_soil, hydpar_wessolek_tab(texture = slb1_soil$texture))

test_that("basic runs using msiterunLWFB90 works",{
  expect_type(msiterunLWFB90(options.b90 = options.b90,
                 param.b90 = param.b90,
                 climate = slb1_meteo,
                 soil = list(soil1 = soil, soil2 = soil),
                 output = -1,
                 rtrn.output = F,
                 rtrn.input = F),
              "list")
  expect_error(msiterunLWFB90(options.b90 = options.b90,
                 param.b90 = param.b90,
                 climate = list(clim1=slb1_meteo, clim2= slb1_meteo),
                 soil = list(soil1 = soil, soil2 = soil, soil3 = soil),
                 output = -1,
                 rtrn.output = F,
                 rtrn.input = F))
})

# test climate-input

clim <- slb1_meteo[year(slb1_meteo$dates) == 2002,]
names(clim)[1] <- "Datum"
climfun <- function(met) {
  names(met)[1] <- "dates"
  met
}

varargs <- list(clim1 = list(met = clim),
                clim2 = list(met=clim))

test_that("msiterun with climate_fun and multiple climate_args works",{
  expect_type(msiterunLWFB90(options.b90 = options.b90,
                             param.b90 = param.b90,
                             climate = climfun,
                             soil = soil,
                             climate_args= varargs,
                             rtrn.output = F,
                             rtrn.input = F),
              "list")
  expect_type(msiterunLWFB90(options.b90 = options.b90,
                 param.b90 = param.b90,
                 climate = climfun,
                 soil = list(soil1 = soil, soil2 = soil),
                 climate_args= varargs,
                 all_combinations = T,
                 output = -1,
                 rtrn.input = F),
              "list")
})

rm(list = ls())


