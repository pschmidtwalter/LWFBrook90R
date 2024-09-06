library(LWFBrook90R)
library(data.table)

# Set up the input data
meteo <- readRDS("tests_compare_actual_vs_previous/meteo.rds")
res_previous <- readRDS("tests_compare_actual_vs_previous/simresults.rds")
model_input <- res_previous$model_input
res_actual <- run_LWFB90(options_b90 = model_input$options_b90,
                         param_b90 = model_input$param_b90,
                         climate = meteo)

test_that("sim-outputs of actual are equal to previous", {
  expect_equal(res_actual$output, res_previous$output, tolerance = 0.001)
  expect_equal(res_actual$layer_output, res_previous$layer_output)
})

test_that("sim-inputs of actual are equal to previous", {
  expect_equal(res_actual$model_input$param_b90, res_previous$model_input$param_b90)
  expect_equal(res_actual$model_input$standprop_daily, res_previous$model_input$standprop_daily)
})
