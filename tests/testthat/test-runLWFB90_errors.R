library(LWFBrook90R)

# Set up the input data
data("slb1_soil")
data("slb1_meteo")
opts <- set_optionsLWFB90(startdate = as.Date("2002-06-01"), enddate = as.Date("2002-06-30"))
soil <- cbind(slb1_soil, hydpar_wessolek_tab(texture = slb1_soil$texture))

# manipulate climate
clim_bad <- slb1_meteo[data.table::year(slb1_meteo$dates) == 2002,]
clim_bad$dates[170] <- as.Date("2002-06-30")

test_that("timelimit works",{
expect_error(
  run_LWFB90(options_b90 = set_optionsLWFB90(startdate = as.Date("1990-01-01")),
          param_b90 = set_paramLWFB90(),
          climate = slb1_meteo,
          soil = soil,
          rtrn.output = FALSE,
          rtrn.input = FALSE,
          timelimit = 0.1)
          )
  })

opts <- set_optionsLWFB90(startdate = as.Date("2002-06-01"), enddate = as.Date("2002-06-30"))

test_that("errorhandling works",{
  expect_error(
    run_LWFB90(options_b90 = opts,
              param_b90 = set_paramLWFB90(),
              climate = clim_bad,
              soil = soil,
              rtrn_input = FALSE, rtrn_output = FALSE)
  )

  expect_error(
    run_LWFB90(options_b90 = opts,
              param_b90 = set_paramLWFB90(psiini = c(rep(-6.3, nrow(soil)-1), 999)),
              climate = slb1_meteo,
              soil = soil,
              rtrn_input = FALSE, rtrn_output = FALSE)
  )
})

rm(list = ls())
