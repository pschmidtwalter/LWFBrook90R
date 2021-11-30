library(LWFBrook90R)
library(data.table)
# Set up the input data
data("slb1_soil")
data("slb1_meteo")
opts <- set_optionsLWFB90(startdate = as.Date("2002-06-01"), enddate = as.Date("2002-06-02"))
parms <- set_paramLWFB90()
soil <- cbind(slb1_soil, hydpar_wessolek_tab(texture = slb1_soil$texture))
output <- set_outputLWFB90()
output[,] <- 1L

# return value (outputs) ----
test_default <- run_LWFB90(options_b90 = opts,
          param_b90 = parms,
          climate = slb1_meteo,
          soil = soil)

test_asc <- run_LWFB90(options_b90 = opts,
                      param_b90 = parms,
                      climate = slb1_meteo,
                      soil = soil,
                      output = output)

test_noinput <- run_LWFB90(options_b90 = opts,
                          param_b90 = parms,
                          climate = slb1_meteo,
                          soil = soil,
                          rtrn_input = F)

test_nooutput <- run_LWFB90(options_b90 = opts,
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

# climate input ---------------------------------

# test climate-input
clim <- slb1_meteo[data.table::year(slb1_meteo$dates) == 2002,]
names(clim)[1] <- "Datum"
climfun <- function(met) {
  names(met)[1] <- "dates"
  met
}

test_that("climate input from function works", {
  expect_type(run_LWFB90(options_b90 = opts,
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
    run_LWFB90(options_b90 = set_optionsLWFB90(startdate = as.Date("2002-01-01"),
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
    run_LWFB90(options_b90 = set_optionsLWFB90(startdate = as.Date("2002-01-01"),
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


meteo <- data.table(slb1_meteo[,c("dates", "tmin", "tmax", "prec", "tmean","vappres", "windspeed", "globrad" )])
meteo <- meteo[year(dates)==2013,]
prec <- meteo[,list(dates, prec = prec*1.1)]
prec_hh_2013 <- readRDS("prec_hh_2013.rds")

test_that("precipitation input works",{

  # input from precip-argument
  res <- run_LWFB90(options_b90 = set_optionsLWFB90(startdate = as.Date("2013-05-14"),
                                             enddate = as.Date("2013-07-28")),
             param_b90 = parms,
             climate = meteo,
             soil = soil,
             precip = prec)

  expect_equal(sum(prec$prec[prec$dates %in% seq.Date(as.Date("2013-05-14"),
                                                      as.Date("2013-07-28"), by = "day")]),
               sum(res$daily_output$rfal+res$daily_output$sfal)
               )

  expect_true(sum(meteo$prec[meteo$dates %in% seq.Date(as.Date("2013-05-14"),
                                                      as.Date("2013-07-28"), by = "day")]) <
                sum(res$daily_output$rfal+res$daily_output$sfal)
              )

  # hourly input
  res <- run_LWFB90(options_b90 = set_optionsLWFB90(startdate = as.Date("2013-05-14"),
                                                    enddate = as.Date("2013-07-28"),
                                                    prec_interval = 24),
                    param_b90 = parms,
                    climate = meteo,
                    soil = soil,
                    precip = prec_hh_2013)
  expect_equal(prec_hh_2013[between(dates, as.Date("2013-05-14"),as.Date("2013-07-28")), sum(prec)],
               sum(res$daily_output$rfal+res$daily_output$sfal))

  })








