library(LWFBrook90R)
library(data.table)

# Set up the input data
data("slb1_soil")
data("slb1_meteo")
parms <- set_paramLWFB90()
soil <- cbind(slb1_soil, hydpar_wessolek_tab(texture = slb1_soil$texture))

# Test precipitation correction -----

test_that("Precipitation correction works", {
  opts <- set_optionsLWFB90(startdate = as.Date("2002-06-01"), enddate = as.Date("2002-06-30"),
                            correct_prec = TRUE)
  withcorr <- run_LWFB90(options_b90 = opts,
                             param_b90 = parms,
                             climate = slb1_meteo,
                             soil = soil)$output$rfal
  opts$correct_prec = FALSE
  withoutcorr <- run_LWFB90(options_b90 = opts,
                    param_b90 = parms,
                    climate = slb1_meteo,
                    soil = soil)$output$rfal

  expect_identical(sum(slb1_meteo$prec[year(slb1_meteo$dates)== 2002 & month(slb1_meteo$dates)==6]),
                   sum(withoutcorr))

  expect_true(sum(withcorr) > sum(withoutcorr))

})

# Test lai_methods -------------------------------------------------------------

# Test root_method -------------------------------------------------------------

# Test use_growthperiod --------------------------------------------------------

# Test standprop_interp --------------------------------------------------------

# Test standprop_input ---------------------------------------------------------

# Test budburst & leaffall methods ---------------------------------------------

# Test prec_interval -----------------------------------------------------------

# Test fornetrad ---------------------------------------------------------------
