library(LWFBrook90R)

# Set up the input data
data("slb1_soil")
data("slb1_meteo")
opts <- setoptions_LWFB90(startdate = as.Date("2002-06-01"), enddate = as.Date("2002-06-30"))
soil <- cbind(slb1_soil, hydpar_wessolek_tab(texture = slb1_soil$texture))

# manipulate climate
clim_bad <- slb1_meteo[data.table::year(slb1_meteo$dates) == 2002,]
clim_bad$dates[170] <- as.Date("2002-06-30")

test_that("timelimit works",{
expect_error(
  runLWFB90(options_b90 = setoptions_LWFB90(startdate = as.Date("1990-01-01")),
          param_b90 = setparam_LWFB90(),
          climate = slb1_meteo,
          soil = soil,
          rtrn.output = FALSE,
          rtrn.input = FALSE,
          timelimit = 0.1)
          )
  })

opts <- setoptions_LWFB90(startdate = as.Date("2002-06-01"), enddate = as.Date("2002-06-30"))

test_that("errorhandling works",{
  expect_error(
    runLWFB90(options_b90 = opts,
              param_b90 = setparam_LWFB90(),
              climate = clim_bad,
              soil = soil,
              rtrn_input = FALSE, rtrn_output = FALSE,
              verbose = TRUE)
  )

  expect_error(
    runLWFB90(options_b90 = opts,
              param_b90 = setparam_LWFB90(psiini = c(rep(-6.3, nrow(soil)-1), 999)),
              climate = slb1_meteo,
              soil = soil,
              rtrn_input = FALSE, rtrn_output = FALSE,
              verbose = TRUE)
  )
})

rm(list = ls())
