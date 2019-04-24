load("H:/Schreiben/EnvModSoft/LWFB90_suppl/DataAndPreparedResults/sample_data.rda")
library(LWFBrook90R)

param.b90 <- setparam_LWFB90()
soils <- soils[order(id, -lower), ]
setDT(clim)
setDT(soils)
clim <- split(clim, by = "id", keep.by = F)
soils <- split(soils, by = "id", keep.by = F)
names(soils)
res <- mrunLWFB90(param.b90 = param.b90,
                  options.b90 = options.b90,
                  soil = soils,
                  climate = clim,
                  multirun.dir = "3veg_multirun")
lapply(res, FUN = function(x) class(x) == "error")


res1 <- unlist(res,recursive = F)
str(res, max.level = 2)
str(res1, max.level = 1)
names(res1[[1]])
str(res1[[1]],max.level = 1)
evapann <- rbindlist(lapply(unlist(res, recursive = F), function(x) (x[["EVAPANN.csv"]])), idcol = "clim_soil_parm_options.combi")
str(evapann

    )


setup_combinations(3,3,1,2, all_combinations = T)
is.data.frame(data.table(soil_list[[1]]))
#######

options.b90 <- setoptions_LWFB90()
param.b90 <- setparam_LWFB90()
soil <- cbind(slb1_soil, hydpar_wessolek_mvg(tex.KA5 = slb1_soil$texture))

soillaymat <- soil_to_param(soil)
param.b90$soil_nodes <- soillaymat$soil_nodes
param.b90$soil_materials <- soillaymat$soil_materials

# data.frame
n = 10


options.b90 <- setoptions_LWFB90(startdate = as.Date("2001-01-01"),
                                 enddate = as.Date("2010-12-31"))
param.b90 <- setparam_LWFB90()
soil <- cbind(slb1_soil, hydpar_wessolek_mvg(tex.KA5 = slb1_soil$texture))
output <- setoutput_LWFB90()
output[,] <- 0
output["Evap", "Ann"] <- 1

N = 10
param_var <- data.frame(glmax  = runif(N,0.0025,0.01),
                        maxlai = runif(N,3,7)
                        )
param_var <- data.frame(cintrl = runif(n, 0.1,0.5),
                        maxlai = runif(n,3,7),
                        soil_materials.ths3 = runif(n,0.3,0.4),
                        soil_materials.ths1 = runif(n,0.4,0.5))

results <- mrunLWFB90(paramvar  = param_var,
                      param.b90 = param.b90,
                      options.b90 = options.b90,
                      climate = slb1_meteo,
                      soil = soil)



args(mrunLWFB90)


