library(LWFBrook90R)
# Run the model (be sure to use the same commands as in the above named R-script.)
# Set up the input data
data("slb1_soil")
data("slb1_meteo")
options.b90 <- setoptions_LWFB90(startdate = as.Date("1990-01-01"))
param.b90 <- setparam_LWFB90()

soil <- cbind(slb1_soil, hydpar_wessolek_tab(texture = slb1_soil$texture))

test_that("timelimit works",{
expect_error(runLWFB90(options.b90 = options.b90,
          param.b90 = param.b90,
          climate = slb1_meteo,
          soil = soil,
          rtrn.output = F,
          rtrn.input = F,
          verbose = FALSE,
          timelimit = 0.5,
          output.log = F))
  })
