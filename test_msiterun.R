load("H:/Schreiben/EnvModSoft/LWFB90_suppl/DataAndPreparedResults/sample_data.rda")
library(LWFBrook90R)


param_l <- list(lai3 = setparam_LWFB90(maxlai = 3),
                lai4 = setparam_LWFB90(maxlai = 4),
                lai5 = setparam_LWFB90(maxlai = 5))
options.b90 <- setoptions_LWFB90()
soils <- soils[order(id, -lower), ]
setDT(clim)
setDT(soils)

clim <- split(clim, by = "id", keep.by = F)
soils <- split(soils, by = "id", keep.by = F)
names(soils)
res <- msiterunLWFB90(param.b90 = param_l,
                  options.b90 = options.b90,
                  soil = soils,
                  climate = clim,
                  multirun.dir = "3veg_multirun")
names(res)
