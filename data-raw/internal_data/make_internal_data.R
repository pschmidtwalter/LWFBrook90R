options(stringsAsFactors = F)

#Wessolek-MVG
wessolek_mvg_tab10 <- read.csv("data-raw/internal_data/wessolek_MVG_tab10.csv")
wessolek_mvg_tab10$mpar <- 1-1/wessolek_mvg_tab10$n
wessolek_mvg_tab10$ksat <- wessolek_mvg_tab10$ksat*10
names(wessolek_mvg_tab10)[c(1,5)] <- c("texture", "npar")


hydpar_forestfloor <- data.frame(ths = 0.848, thr = 0, alpha = 98, npar = 1.191,
                         mpar=0.1603694, ksat = 98000,tort = 0.5, stringsAsFactors = F)


# # hypres tab
hypres_tab4 <- read.csv("data-raw/internal_data/HypresKlassPTF.csv", stringsAsFactor=F)
names(hypres_tab4) <- c("texture", "topsoil", "ths", "thr", "alpha", "npar","mpar","ksat", "tort")
hypres_tab4$topsoil <- as.logical(hypres_tab4$topsoil)
hypres_tab4 <- rbind(hypres_tab4, hypres_tab4[hypres_tab4$tex.hypres=="Org",])
hypres_tab4$topsoil[11] <- TRUE
row.names(hypres_tab4) <- NULL
#output_function <- function(x, tolayer) {
# aggregate SWAT
# # teepe-table
# teepe_tables123 <- read.csv("data-raw/TeepePTF.csv", stringsAsFactors=F)
# str(teepe_tables123)
# names(teepe_tables123) <- c("bd.teepe", "tex.teepe", "AC", "AWC", "PWP", "mean_oc", "AC_surcharge",
#                             "AWC_surcharge","PWP_surcharge","ths", "n","alpha","thr")
# teepe_tables123$m <- 1-1/teepe_tables123$n
# teepe_tables123$thr <- teepe_tables123$thr/100
#devtools::use_data(teepe_tables123, hypres_tab4,wessolek_mvg_tab10,din4220_tabA1, internal =T, overwrite = T)

# Vignette data -----

library(LWFBrook90R)
library(data.table)
# b90res ---------------
options_b90 <- set_optionsLWFB90()
param_b90 <- set_paramLWFB90()
soil <- cbind(slb1_soil, hydpar_wessolek_tab(texture = slb1_soil$texture))
output <- set_outputLWFB90()

b90res <- run_LWFB90(options_b90 = options_b90,
                    param_b90 = param_b90,
                    climate = slb1_meteo,
                    soil = soil,
                    output = output)


# mrun_res -------------

# Agg-Function
output_function <- function(x, tolayer) {
  # aggregate SWAT
  swat_tran <- x$SWATDAY.ASC[which(nl <= tolayer),
                             list(swat = sum(swati)),
                             by  = list(yr, doy)]
  #add transpiration from EVAPDAY.ASC
  swat_tran$tran <- x$EVAPDAY.ASC$tran

  # get beginning and end of growing season from input parameters
  vpstart <- x$model_input$param_b90$budburstdoy
  vpend <- x$model_input$param_b90$leaffalldoy
  swat_tran <- merge(swat_tran,
                     data.frame(yr = unique(swat_tran$yr),
                                vpstart, vpend), by = "yr")
  # mean swat and tran sum
  swat_tran[doy >= vpstart & doy <= vpend,
            list(swat_vp_mean = mean(swat), tran_vp_sum = sum(tran)), by = yr]
}

N=50
set.seed(2021)
paramvar <- data.frame(maxlai = runif(N, 4,7),
                       glmax = runif(N,0.003, 0.01))

mrun_res <- run_multi_LWFB90(paramvar = paramvar,
                             param_b90 = set_paramLWFB90(),
                             cores = 3, # arguments below are passed to run_LWFB90()
                             options_b90 = set_optionsLWFB90(),
                             climate = slb1_meteo,
                             soil = soil,
                             output = set_outputLWFB90(),
                             rtrn_input = FALSE, rtrn_output = FALSE,
                             output_fun = output_function,
                             tolayer = 15)

mrun_dt <- rbindlist(lapply(mrun_res, function(x) x$output_fun[[1]]),
                     idcol = "singlerun")


#speichert den Dataframe als internes Objekt, welches nicht exportiert wird. ANsprechen mit brook90r:::wess_mvg_tex
usethis::use_data(mrun_dt, b90res, wessolek_mvg_tab10,hydpar_forestfloor, hypres_tab4, internal = T, overwrite =T)

#rm(list = ls())
