load("H:/Schreiben/EnvModSoft/LWFB90_suppl/DataAndPreparedResults/sample_data.rda")
library(LWFBrook90R)
options.b90 <- setoptions_LWFB90()
param.b90 <- setparam_LWFB90()

options_list <- list(options.b90,options.b90,options.b90)

paramlist <- list(no1 = param.b90, no2 = param.b90, no3 =param.b90)
str(param.b90, max.level = 1)
str(paramlist,max.level = 1)

clim_list <- split(clim, by = "id", keep.by = F)
soil_list <- split(soils, by = "id", keep.by = F)

res <- mrunLWFB90(param.b90 = paramlist,
                  options.b90 = options_list,
                  soil = soil_list[1:2],
                  climate = clim_list[1:2],
                  all_combination = F,
                  multirun.dir = "test")
str(res, max.level =2)

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
paramvar <- data.frame(cintrl = runif(n, 0.1,0.5),
                       maxlai = runif(n,3,7),
                        soil_materials.ths3 = runif(n,0.3,0.4),
                        soil_materials.ths1 = runif(n,0.4,0.5))

results <- mrunLWFB90(param.b90 = param.b90,
           options.b90 = options.b90,
          climate = slb1_meteo,
          paramvar = paramvar)



args(mrunLWFB90)


