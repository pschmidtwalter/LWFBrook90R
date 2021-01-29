library(LWFBrook90R)
library(data.table)
data("slb1_soil")
data("slb1_meteo")
opts <- set_optionsLWFB90(startdate = as.Date("2002-06-01"), enddate = as.Date("2002-06-02"))
parms <- set_paramLWFB90()
soil <- cbind(slb1_soil, hydpar_wessolek_tab(texture = slb1_soil$texture))

test_that("basic runs using run_multisite_LWFB90 works",{
  expect_type(run_multisite_LWFB90(options_b90 = opts,
                 param_b90 = parms,
                 climate = slb1_meteo,
                 soil = list(soil1 = soil, soil2 = soil),
                 output = -1,
                 rtrn_output = FALSE,
                 rtrn_input = FALSE),
              "list")
  expect_error(run_multisite_LWFB90(options_b90 = opts,
                 param_b90 = parms,
                 climate = list(clim1=slb1_meteo, clim2= slb1_meteo),
                 soil = list(soil1 = soil, soil2 = soil, soil3 = soil),
                 output = -1,
                 rtrn_output = F,
                 rtrn_input = F))
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
  expect_type(run_multisite_LWFB90(options_b90 = opts,
                             param_b90 = parms,
                             climate = climfun,
                             soil = soil,
                             climate_args= varargs,
                             rtrn_output = F,
                             rtrn_input = F),
              "list")
  expect_type(run_multisite_LWFB90(options_b90 = opts,
                 param_b90 = parms,
                 climate = climfun,
                 soil = list(soil1 = soil, soil2 = soil),
                 climate_args= varargs,
                 all_combinations = T,
                 output = -1,
                 rtrn_input = F),
              "list")
})

detach("package:data.table", unload = TRUE)


