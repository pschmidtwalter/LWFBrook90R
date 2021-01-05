library(LWFBrook90R)

# Set up the input data
data("slb1_soil")
data("slb1_meteo")
opts <- setoptions_LWFB90(startdate = as.Date("2002-06-01"), enddate = as.Date("2002-06-02"))
parms <- setparam_LWFB90()
soil <- cbind(slb1_soil, hydpar_wessolek_tab(texture = slb1_soil$texture))
output <- setoutput_LWFB90()
output[,] <- 1L

# return value (outputs) ----
test_default <- runLWFB90(options_b90 = opts,
          param_b90 = parms,
          climate = slb1_meteo,
          soil = soil)

test_asc <- runLWFB90(options_b90 = opts,
                      param_b90 = parms,
                      climate = slb1_meteo,
                      soil = soil,
                      output = output)

test_noinput <- runLWFB90(options_b90 = opts,
                          param_b90 = parms,
                          climate = slb1_meteo,
                          soil = soil,
                          rtrn_input = F)

test_nooutput <- runLWFB90(options_b90 = opts,
                          param_b90 = parms,
                          climate = slb1_meteo,
                          soil = soil,
                          rtrn_output = F)

test_that("input/output complete",{

  # default
  expect_type(test_default$daily_output, "list")
  expect_type(test_default$layer_output, "list")
  expect_type(test_default$model_input, "list")
  expect_type(test_default$model_input$param_b90, "list")
  expect_type(test_default$model_input$options_b90, "list")
  expect_type(test_default$model_input$standprop_daily, "list")

  # no input
  expect_type(test_noinput$daily_output, "list")
  expect_type(test_noinput$layer_output, "list")
  expect_null(test_noinput$model_input)

  # no output
  expect_null(test_nooutput$daily_output)
  expect_null(test_nooutput$layer_output)

  # asc output objects
  expect_identical(names(test_asc)[grepl("ASC", names(test_asc))],
                   c("BUDGDAY.ASC","BUDGMON.ASC","BUDGANN.ASC","FLOWDAY.ASC","FLOWMON.ASC",
                     "FLOWANN.ASC","EVAPDAY.ASC","EVAPMON.ASC","EVAPANN.ASC","ABOVDAY.ASC",
                     "ABOVMON.ASC","ABOVANN.ASC","BELODAY.ASC","BELOMON.ASC","BELOANN.ASC",
                     "SWATDAY.ASC","SWATMON.ASC","SWATANN.ASC","MISCDAY.ASC","MISCMON.ASC","MISCANN.ASC")
  )
})

# climate input ----

# test climate-input
clim <- slb1_meteo[data.table::year(slb1_meteo$dates) == 2002,]
names(clim)[1] <- "Datum"
climfun <- function(met) {
  names(met)[1] <- "dates"
  met
}

test_that("climate input from function works", {
  expect_type(runLWFB90(options_b90 = opts,
                        param_b90 = parms,
                        soil = soil,
                        climate = climfun,
                        met = clim),"list")
})

# output function ----
# define some output functions that taps output AND input
outfun1 <- function(x) {
  vpstart <- x$model_input$param_b90$budburstdoy
  vpend <- x$model_input$param_b90$leaffalldoy
  x$daily_output[doy >= vpstart & doy <= vpend, list(tranvp = sum(tran)) ]
}
outfun2 <- function(x) {
  vpstart <- x$model_input$param_b90$budburstdoy
  vpend <- x$model_input$param_b90$leaffalldoy
  x$daily_output[doy >= vpstart & doy <= vpend, list(ptranvp = sum(ptran)) ]
}

test_that("single output functions works", {
  expect_type(
    runLWFB90(options_b90 = setoptions_LWFB90(startdate = as.Date("2002-01-01"),
                                              enddate = as.Date("2002-12-31")),
              param_b90 = parms,
              soil = soil,
              climate = climfun,
              met = clim,
              output_fun = outfun1,
              rtrn_input =F,
              rtrn_output = F)$output_fun,
              "list")
  expect_type(
    runLWFB90(options_b90 = setoptions_LWFB90(startdate = as.Date("2002-01-01"),
                                              enddate = as.Date("2002-12-31")),
              param_b90 = parms,
              soil = soil,
              climate = climfun,
              met = clim,
              output_fun = list(out1 = outfun1, out2 = outfun2),
              rtrn_input =F,
              rtrn_output = F)$output_fun,
    "list")
})

rm(list = ls())




