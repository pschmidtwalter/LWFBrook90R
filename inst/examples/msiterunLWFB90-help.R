options.b90 <- setoptions_LWFB90(budburst.method = "Menzel")

# define parameter sets
param_l <- list(spruce = setparam_LWFB90(maxlai = 5,
                                         budburst.species = "Picea abies (frueh)",
                                         winlaifrac = 0.8),
                beech = setparam_LWFB90(maxlai = 6,
                                        budburst.species = "Fagus sylvatica",
                                        winlaifrac = 0))

soil <- cbind(slb1_soil, hydpar_wessolek_tab(slb1_soil$texture))

# define list of soil objects
soils <- list(soil1 = soil, soil2 = soil)

# define list of climate objects
climates <- list(clim1 = slb1_meteo, clim2 = slb1_meteo)

# run two parameter sets on a series of climate and soil-objects
# (run = FALSE: 'dry' run without actual simulation)
res <- msiterunLWFB90(param.b90 = param_l,
                      options.b90 = options.b90,
                      soil = soils,
                      climate = climates,
                      run = FALSE)

names(res)

# all possible combinations of soil, climate, parameters
res <- msiterunLWFB90(param.b90 = param_l,
                      options.b90 = options.b90,
                      soil = soils,
                      climate = climates,
                      all_combinations = TRUE,
                      run = FALSE)
names(res)
