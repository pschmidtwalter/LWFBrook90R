# Manual test comparing the simulation results of the actual version vs a previous reference,
# to check if some unwanted changes have been made
# it will work savely only, if actual and previous results were created on the same machine.
# machine: oflks593
# ref-version: v0.5.3

library(LWFBrook90R)
library(data.table)

res_previous <- readRDS("tests_compare_actual_vs_previous/simresults.rds")
meteo <- readRDS("tests_compare_actual_vs_previous/meteo.rds")
input <- res_previous$model_input
setDT(meteo)

input$param_b90$water_table_depth <- -9999 # new input-parameters
input$param_b90$snowlqini <- 0
input$param_b90$snowccini <- 0

res_actual <- run_LWFB90(options_b90 = input$options_b90,
                         param_b90 = input$param_b90,
                         climate = meteo)

res_previous$layer_output <- res_previous$layer_output[order(nl, yr, doy)]

test_that("sim-outputs of actual are equal to previous", {
  expect_equal(res_actual$output[,-which(names(res_actual$output) %in% c("snowlq", "cc")), with = F], res_previous$output)
  expect_equal(res_actual$layer_output[,-which(names(res_actual$layer_output) %in% c("relawati")), with = F], res_previous$layer_output)
})

test_that("sim-inputs of actual are equal to previous", {
  expect_equal(res_actual$model_input$param_b90, res_previous$model_input$param_b90)
  expect_equal(res_actual$model_input$standprop_daily, res_previous$model_input$standprop_daily)
})
