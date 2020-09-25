
# ---- load test data ---------------------------------------------------------------
# b90.exe output produced using 'data-raw/data_for_testing/b90exe_output/create_b90exe_refdat.R'
load("data-raw/data_for_testing/b90exe_output/exe_output.rda")

# Run the model (be sure to use the same commands as in the above named R-script.)
# Set up the input data
data("slb1_soil")
data("slb1_meteo")
options.b90 <- setoptions_LWFB90()
param.b90 <- setparam_LWFB90()

soil <- cbind(slb1_soil, hydpar_wessolek_tab(tex.KA5 = slb1_soil$texture))

# produce model input
b90res <- runLWFB90(options.b90 = options.b90,
                    param.b90 = param.b90,
                    climate = slb1_meteo,
                    soil = soil,
                    output = -1,
                    verbose = FALSE,
                    output.log = T)
b90res$daily_output <- b90res$daily_output[,names(exe_dout), with =F]
b90res$layer_output <- b90res$layer_output[,names(exe_lout), with =F]

test_that("inputs are equal", {
  expect_equal(b90res$model_input$options.b90,pkg_input$options.b90)
  expect_equal(b90res$model_input$param.b90,pkg_input$param.b90)
  expect_equal(b90res$model_input$standprop_daily,pkg_input$standprop_daily)
})

test_that("daily outputs are equal", {
  for (nm in names(b90res$daily_output)) {
    expect_equal(b90res$daily_output[[nm]], exe_dout[[nm]], tolerance = 1)
  }
})

test_that("layer outputs are equal", {
  for (nm in names(b90res$layer_output)) {
    expect_equal(b90res$layer_output[[nm]], exe_lout[[nm]], tolerance = 1)
  }
})
