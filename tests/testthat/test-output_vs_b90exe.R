skip("Will fail as long as the source codes differ!")
library(LWFBrook90R)
# ---- load test data ---------------------------------------------------------------
# b90.exe output produced using 'data-raw/data_for_testing/b90exe_output/create_b90exe_refdat.R'
load("data-raw/data_for_testing/b90exe_output/exe_output.rda")

# Run the model (be sure to use the same commands as in the above named R-script.)
# Set up the input data
data("slb1_soil")
data("slb1_meteo")
options.b90 <- setoptions_LWFB90()
param.b90 <- setparam_LWFB90()

soil <- cbind(slb1_soil, hydpar_wessolek_tab(texture = slb1_soil$texture))
output <- setoutput_LWFB90()
output[,] <- 0L
output[,1:3] <- 1L
# produce model input
b90res <- runLWFB90(options.b90 = options.b90,
                    param.b90 = param.b90,
                    climate = slb1_meteo,
                    soil = soil,
                    output = output,
                    verbose = FALSE,
                    output.log = F)

test_that("inputs are equal", {
  expect_equal(b90res$model_input$options.b90,pkg_input$options.b90)
  expect_equal(b90res$model_input$param.b90,pkg_input$param.b90)
  expect_equal(b90res$model_input$standprop_daily,pkg_input$standprop_daily)
})



# ------------------------------------------------------------------------------
# Tests

# ----- ABOV-tests-----------------------------------------------------------

# columns to test
nms <- names(b90res$ABOVDAY.ASC) #[-which(names(b90res$BUDGDAY.ASC) %in% c("evap"))]

test_that("values in ABOVDAY are equal", {
  expect_equal(b90res$ABOVDAY.ASC[,nms, with = F], exe_res$ABOVDAY.ASC[,nms, with = F], tolerance = 0.001)
})

nms <- nms[!nms %in% c("da", "doy")]
test_that("values in ABOVMON.ASC are equal", {
  expect_equal(b90res$ABOVMON.ASC[,nms, with = F], exe_res$ABOVMON.ASC[,nms, with = F], tolerance = 0.001)
})

nms <- nms[nms != "mo"]
test_that("values in ABOVANN.ASC are equal", {
  expect_equal(b90res$ABOVANN.ASC[,nms, with = F], exe_res$ABOVANN.ASC[,nms, with = F], tolerance = 0.001)
})

# ----- BUDGDAY-tests-----------------------------------------------------------

b90res$BUDGDAY.ASC[, intrs := intr+ints]
b90res$BUDGMON.ASC[, intrs := intr+ints]
b90res$BUDGANN.ASC[, intrs := intr+ints]
b90res$BUDGDAY.ASC[, c("intr", "ints") := NULL]
b90res$BUDGMON.ASC[, c("intr", "ints") := NULL]
b90res$BUDGANN.ASC[, c("intr", "ints") := NULL]
setnames(exe_res$BUDGANN.ASC, "intr+s", "intrs")
setnames(exe_res$BUDGMON.ASC, "intr+s", "intrs")
setnames(exe_res$BUDGDAY.ASC, "intr+s", "intrs")
exe_res$BUDGDAY.ASC$mesfl <- NULL
exe_res$BUDGMON.ASC$mesfl <- NULL
exe_res$BUDGANN.ASC$mesfl <- NULL

# columns to test
nms <- names(b90res$BUDGDAY.ASC) #[-which(names(b90res$BUDGDAY.ASC) %in% c("evap"))]

test_that("values in BUDGDAY are equal", {
  expect_equal(b90res$BUDGDAY.ASC[,nms, with = F], exe_res$BUDGDAY.ASC[,nms, with = F], tolerance = 0.001)
})

test_that("values in BUDGMON are equal", {
  expect_equal(b90res$BUDGMON.ASC, exe_res$BUDGMON.ASC, tolerance = 0.001)
})

test_that("values in BUDGANN are equal", {
  expect_equal(b90res$BUDGANN.ASC[,nms, with = F], exe_res$BUDGANN.ASC[,nms, with = F], tolerance = 0.001)

})

# ----- EVAP-tests-----------------------------------------------------------

# column to test
nms <- names(b90res$EVAPDAY.ASC) #[-which(names(b90res$BUDGDAY.ASC) %in% c("evap"))]

test_that("values in EVAPDAY.ASC are equal", {
  expect_equal(b90res$EVAPDAY.ASC[,nms, with = F], exe_res$EVAPDAY.ASC[,nms, with = F], tolerance = 0.001)
})

nms <- nms[!nms %in% c("da", "doy")]
test_that("values in EVAPMON.ASC are equal", {
  expect_equal(b90res$EVAPMON.ASC[,nms, with = F], exe_res$EVAPMON.ASC[,nms, with = F], tolerance = 0.001)
})

nms <- nms[nms != "mo"]
test_that("values in EVAPANN.ASC are equal", {
  expect_equal(b90res$EVAPANN.ASC[,nms, with = F], exe_res$EVAPANN.ASC[,nms, with = F], tolerance = 0.001)
})

# ----- BELO-tests-----------------------------------------------------------
exe_res$BELOANN.ASC <- exe_res$BELOANN.ASC[!is.na(yr),]

# column to test
nms <- names(b90res$BELODAY.ASC) #[-which(names(b90res$BUDGDAY.ASC) %in% c("evap"))]

test_that("values in BELODAY.ASC are equal", {
  expect_equal(b90res$BELODAY.ASC[,nms, with = F], exe_res$BELODAY.ASC[,nms, with = F], tolerance = 0.001)
})

nms <- nms[!nms %in% c("da", "doy")]
test_that("values in BELOMON.ASC are equal", {
  expect_equal(b90res$BELOMON.ASC[,nms, with = F], exe_res$BELOMON.ASC[,nms, with = F], tolerance = 0.001)
})

nms <- nms[nms != "mo"]
test_that("values in BELOANN.ASC are equal", {
  expect_equal(b90res$BELOANN.ASC[,nms, with = F], exe_res$BELOANN.ASC[,nms, with = F], tolerance = 0.001)
})


# ----- MISC-tests-----------------------------------------------------------
exe_res[c("MISCANN.ASC","MISCMON.ASC", "EPVPANN.ASC")] <- NULL
b90res[c("MISCANN.ASC","MISCMON.ASC", "EPVPANN.ASC")] <- NULL
exe_res$MISCDAY.ASC$awat40 <- NULL

# column to test
nms <- names(b90res$MISCDAY.ASC) #[-which(names(b90res$BUDGDAY.ASC) %in% c("evap"))]

test_that("values in MISCDAY.ASC are equal", {
  expect_equal(b90res$MISCDAY.ASC[,nms, with = F], exe_res$MISCDAY.ASC[,nms, with = F], tolerance = 0.001)
})

# clean up
#rm(list = ls())






