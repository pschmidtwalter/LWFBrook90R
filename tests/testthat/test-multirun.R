library(LWFBrook90R)
library(data.table)
data("slb1_soil")
data("slb1_meteo")

if (parallelly::availableCores() > 1) {
  cores <- 2
} else {
  cores <- 1
}

soil <- cbind(slb1_soil, hydpar_wessolek_tab(texture = slb1_soil$texture))
soil_lay_mat <- soil_to_param(soil)

opts <- set_optionsLWFB90(startdate = as.Date("2002-12-15"), enddate = as.Date("2003-01-15"))
parms <- set_paramLWFB90(sai = c(0.5,1.2),maxlai = c(2,5),
                         soil_nodes = soil_lay_mat$soil_nodes,
                         soil_materials = soil_lay_mat$soil_materials)


vary_parms <- data.frame(maxlai = c(0.941, 0.952),
                         sai2 = c(0.951, 0.952),
                         fsintlai = c(0.961,0.962),
                         frintsai = c(0.971, 0.972),
                         fsintsai = c(0.981, 0.982),
                         soil_materials.ths2 = c(0.991,0.992))

res_multi <- run_multi_LWFB90(paramvar = vary_parms,
                              param_b90 = parms,
                              options_b90 = opts,
                              climate = slb1_meteo)


used_singleparms = rbindlist(
  lapply(res_multi, function(x) {
    x$model_input$param_b90[c("fsintlai","frintsai", "fsintsai" )]
  }))

used_annual_vegparms = rbindlist(lapply(res_multi, function(x) {
  x$model_input$param_b90[c("maxlai", "sai")]
}), idcol="No.")

used_listparms = unlist(lapply(res_multi, function(x) {
  x$model_input$param_b90$soil_materials$ths[2]
}), use.names = FALSE)

setDT(vary_parms)
test_that("multi-run: all vary_parms at the right place",{
  expect_equal(used_singleparms,vary_parms[,names(used_singleparms), with = F])
  expect_equal(used_annual_vegparms$maxlai, rep(vary_parms$maxlai, each = 2))
  expect_equal(used_annual_vegparms$sai[c(2,4)], vary_parms$sai2)
  expect_equal(used_listparms, vary_parms$soil_materials.ths2)
})
