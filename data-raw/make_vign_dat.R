library(LWFBrook90R)
library(data.table)
# b90res ---------------
options.b90 <- setoptions_LWFB90()
param.b90 <- setparam_LWFB90()
soil <- cbind(slb1_soil, hydpar_wessolek_tab(tex.KA5 = slb1_soil$texture))
output <- setoutput_LWFB90()

b90res <- runLWFB90(options.b90 = options.b90,
                    param.b90 = param.b90,
                    climate = slb1_meteo,
                    soil = soil,
                    output = output, output.log = F)



# mrun_res -------------
# Agg-Function

output_function <- function(x) {
  # aggregate SWAT
  swat_tran <- x$SWATDAY.ASC[which(nl <= 14),
                             list(swat100cm = sum(swati)),
                             by  = list(dates = as.Date(paste(yr, mo, da, sep = "-")))]
  #add transpiration from EVAPDAY.ASC
  swat_tran$tran <- x$EVAPDAY.ASC$tran
  return(swat_tran)
}

N=50
paramvar <- data.frame(maxlai = runif(N, 4,7),
                       glmax = runif(N,0.003, 0.01))

mrun_res <- mrunLWFB90(paramvar = paramvar,
                       param.b90 = param.b90,
                       cores = 3,
                       options.b90 = options.b90, # the following args are passed to runLWFB90
                       climate = slb1_meteo,
                       soil = soil,
                       output = output,
                       rtrn.input = F, rtrn.output = F,
                       output_fun = output_function,
                       output.log = F, verbose = F)

# vignette data: store in data-raw and include internal data with script 'make_internal_data'
save(mrun_res, b90res, file = "data-raw/vign_dat.rda")


