# This file creates and saves simulation results of the actual version ', in
# order to test them for differences of the next version.
library(LWFBrook90R)
library(data.table)

# Set up the input data
data("slb1_soil")
data("slb1_meteo")

# create model input
opts <- set_optionsLWFB90(startdate = as.Date("2002-01-01"), enddate = as.Date("2003-12-31"))
parms <- set_paramLWFB90()
soil <- cbind(slb1_soil, hydpar_wessolek_tab(texture = slb1_soil$texture))
setDT(slb1_meteo)
meteo <- slb1_meteo[which(dates >= opts$startdate
                          & dates <= opts$enddate),]

res <- run_LWFB90(options_b90 = opts, param_b90 = parms, climate = meteo, soil =soil)

saveRDS(res, file = "tests_compare_actual_vs_previous/simresults.rds")
saveRDS(meteo, file = "tests_compare_actual_vs_previous/meteo.rds")
