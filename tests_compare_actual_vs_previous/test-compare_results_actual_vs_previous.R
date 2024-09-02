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

res_actual <- run_LWFB90(options_b90 = opts, param_b90 = parms, climate = meteo, soil =soil)

meteo <- readRDS("tests_compare_actual_vs_previous/meteo.rds")
res_previous <- readRDS("tests_compare_actual_vs_previous/simresults.rds")

test_that("sim-outputs of actual are equal to previous", {
  expect_equal(res_actual$output, res_previous$output)
  expect_equal(res_actual$layer_output, res_previous$layer_output)
})


test_that("sim-inputs of actual are equal to previous", {
  expect_equal(res_actual$model_input$param_b90, res_previous$model_input$param_b90)
  expect_equal(res_actual$model_input$standprop_daily, res_previous$model_input$standprop_daily)
})
