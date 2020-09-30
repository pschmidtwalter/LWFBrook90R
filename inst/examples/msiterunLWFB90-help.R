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
# (run = FALSE: 'dry' run without actual simulation, only input returned)
res <- msiterunLWFB90(param.b90 = param_l,
                      options.b90 = options.b90,
                      soil = soils,
                      climate = climates,
                      run = FALSE)
names(res)

# set up and run individual parameter sets for individual locations

# set up location parameters
loc_parm <- data.frame(loc_id = names(climates),
                       coords_y = c(48.0, 54.0),
                       eslope = c(30,0),
                       aspect = c(180,0))

# create input list of multiple param.b90 list objects
param_l <- lapply(names(climates), function(x, loc_parms) {
  parms <- setparam_LWFB90()
  parms[match(names(loc_parm),names(parms), nomatch = 0)] <-
    loc_parm[loc_parm$loc_id == x, which(names(loc_parm) %in% names(parms))]
  parms
}, loc_parm = loc_parm)

names(param_l) <- c("locpar1", "locpar2")

res <- msiterunLWFB90(param.b90 = param_l,
                      options.b90 = options.b90,
                      soil = soils,
                      climate = climates,
                      run = FALSE)
names(res)
