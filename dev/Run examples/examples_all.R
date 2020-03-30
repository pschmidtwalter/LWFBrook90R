library(LWFBrook90R)
rm(list=ls())
# -----runLWBB90  ----
#Set up lists containing model control options and model parameters:
param.b90 <- setparam_LWFB90()
options.b90 <- setoptions_LWFB90()

# Set start and end Dates for the simulation
options.b90$startdate <- as.Date("2002-01-01")
options.b90$enddate <- as.Date("2003-12-31")

# Derive soil hydraulic properties from soil physical properties
# using pedotransfer functions
soil <- cbind(slb1_soil, hydpar_wessolek_mvg(slb1_soil$texture))

# Run LWF-Brook90
b90.result <- runLWFB90(project.dir = "example_run_b90",
                        options.b90 = options.b90,
                        param.b90 = param.b90,
                        climate = slb1_meteo,
                        soil = soil)

# use a function to be performed on the output:
# aggregate soil water storage down to a specific layer
agg_swat <- function(x, layer) {
  out <- aggregate(SWATI~YR+DOY,
                   x$SWATDAY.ASC,
                   FUN = sum,
                   subset = NL <= layer)
  out[order(out$YR, out$DOY),]}

# run model, without returning the original output.
b90.aggswat <- runLWFB90(project.dir = "example_run_b90",
                         options.b90 = options.b90,
                         param.b90 = param.b90,
                         climate = slb1_meteo,
                         soil = soil,
                         output_fun = list(swat = agg_swat),
                         rtrn.output = F,
                         layer = 10) # passed to output_fun
str(b90.aggswat$output_fun$swat)

# ---- mrunLWFB90  ----
rm(list=ls())
#Set up lists containing model control options and model parameters:
param.b90 <- setparam_LWFB90()
options.b90 <- setoptions_LWFB90()

# Derive soil hydraulic properties from soil physical properties using pedotransfer functions
soil <- cbind(slb1_soil, hydpar_wessolek_mvg(slb1_soil$texture))

# Set start and end Dates for the simulation
options.b90$startdate <- as.Date("2002-01-01")
options.b90$enddate <- as.Date("2003-12-31")

# choose the 'Coupmodel' shape option for the annual lai dynamic, with fixed budburst and leaf fall dates:
options.b90$lai.method <- 'Coupmodel'
options.b90$budburst.method <- 'fixed'
options.b90$leaffall.method <- 'fixed'

#set up data.frame with variable parameters
n <- 5
vary_parms <- data.frame(shape.optdoy = runif(n,180,240),
                         shape.budburst = runif(n, 0.1,1),
                         winlaifrac = runif(n, 0,0.5),
                         budburstdoy = runif(n,100,150),
                         soil_materials.ths3 = runif(n, 0.3,0.5), # ths of material 3
                         maxlai2 = runif(n,4,8)) # 2nd year lai

# soil as soil_nodes and soil materials to param.b90, so ths3 can be looked up
param.b90[c("soil_nodes", "soil_materials")] <- soil_to_param(soil)
# set up maxlai with length 2, so maxlai2 of paramvar can be looked up
param.b90$maxlai <- c(5, 5)

# Make a Multirun-Simulation
b90.multi <- mrunLWFB90(paramvar = vary_parms,
                        param.b90 = param.b90,
                        options.b90 = options.b90,
                        climate = slb1_meteo)
names(b90.multi)

#extract results: EVAPDAY.ASC
evapday <- data.table::rbindlist(lapply(b90.multi,
                                        FUN = function(x) {x[["EVAPDAY.ASC"]]}),
                                 idcol = "srun")
evapday$dates <- with(evapday, as.Date(paste(YR,MO,DA), "%Y %m %d"))

srun_nms <- unique(evapday$srun)

with(evapday[evapday$srun == srun_nms[1], ],
     plot(dates, cumsum(EVAP), type = "l")
)
for (i in 2:length(b90.multi)){
  with(evapday[evapday$srun == srun_nms[i], ],
  lines(dates, cumsum(EVAP)))
}

# ---- msiterunLWFB90 -----
rm(list=ls())

options.b90 <- setoptions_LWFB90(budburst.method = "Menzel")

# define parameter sets
param_l <- list(spruce = setparam_LWFB90(maxlai = 5, budburst.species = "Picea abies (frueh)", winlaifrac = 0.8),
                beech = setparam_LWFB90(maxlai = 6, budburst.species = "Fagus sylvatica", winlaifrac = 0))

soil <- cbind(slb1_soil, hydpar_wessolek_mvg(slb1_soil$texture))

# define list of soil objects
soils <- list(soil1 = soil, soil2 = soil)

# define list of climate objects
climates <- list(clim1 = slb1_meteo, clim2 = slb1_meteo)

# run two parameter sets on a series of climate and soil-objects
res <- msiterunLWFB90(param.b90 = param_l,
                      options.b90 = options.b90,
                      soil = soils,
                      climate = climates)
names(res)

#  all combinations
res <- msiterunLWFB90(param.b90 = param_l,
                      options.b90 = options.b90,
                      soil = soils,
                      climate = climates,
                      all_combinations = T)
names(res)


# ---- MakeStand  ----

options.b90 <- setoptions_LWFB90()
param.b90 <- setparam_LWFB90()

standprop <- make_standprop(options.b90,
                            param.b90,
                            out.years = 2002:2004)
plot(standprop$dates, standprop$lai, type = "l")

rm(list=ls())

# --- approx_standprop  ----
options.b90 <- setoptions_LWFB90()
param.b90 <- setparam_LWFB90()

years <- 2002:2004
options.b90$standprop.interp <- 'constant'
param.b90$height <- c(20.2,20.8,21.3)
height_c <- approx_standprop(x.years=years,
                             y = param.b90$height,
                             approx.method = options.b90$standprop.interp)

# linear interpolation
options.b90$standprop.interp <- 'linear'
param.b90$height.ini <- 19.1
height_l <- approx_standprop(x.years=years,
                             y = param.b90$height,
                             y.ini = param.b90$height.ini,
                             approx.method = options.b90$standprop.interp)

# use growthperiod
options.b90$standprop.use_growthperiod <- TRUE
height_l_gp <- approx_standprop(x.years = years,
                                y = param.b90$height,
                                y.ini = param.b90$height.ini,
                                use_growthperiod = options.b90$standprop.use_growthperiod,
                                startdoy = param.b90$budburstdoy,
                                enddoy = param.b90$leaffalldoy,
                                approx.method = options.b90$standprop.interp)

dates <- seq.Date(from = as.Date(paste0(min(years),"-01-01")),
                  to = as.Date(paste0(max(years),"-12-31")),
                  by = "day")
plot(dates, height_c,
     type = "l", lwd = 2, col = "black",
     ylim = c(19,22), ylab = "height [m]", xlab = "", xpd = T)
lines(dates, height_l,
      col = "blue", lwd = 2)
lines(dates, height_l_gp,
      col = "green", lwd = 2)
legend("topleft", legend = c("'constant'",
                             "'linear'", "'linear', 'use_growthperiod'"),
       col  = c("black", "blue", "green"),  lwd = 2, pch = NULL,
       bty = "n")

rm(list=ls())

# ---- calc_gof  ----
#Set up lists containing model control options and model parameters:
param.b90 <- setparam_LWFB90()
options.b90 <- setoptions_LWFB90()

# Set start and end Dates for the simulation
options.b90$startdate <- as.Date("2006-01-01")
options.b90$enddate <- as.Date("2006-12-31")

# Derive soil hydraulic properties from soil physical properties
# using pedotransfer functions
soil <- cbind(slb1_soil, hydpar_wessolek_mvg(slb1_soil$texture))

# Run LWF-Brook90
b90.result <- runLWFB90(project.dir = "example_run_b90",
                        options.b90 = options.b90,
                        param.b90 = param.b90,
                        climate = slb1_meteo,
                        soil = soil)

# prepare observations
observations <- slb1_mpot #daily water potential in different soil depths
names(observations)[2:6] <- c("psimi5", "psimi7", "psimi10", "psimi16","psimi21")
observations <- observations[observations$dates >= options.b90$startdate
                             & observations$dates <= options.b90$enddate,]

# calculate gof-measure using simulation results and observations
# simulated water tension is in kPa, observed in hPa
calc_gof(obs = observations,
         b90.result,
         gof_fun = function(sim, obs) {mean(obs-sim/10, na.rm = T)})

# multiple gof-measures
calc_gof(obs = observations, b90.result,
         gof_fun = list(
           rmse = function(sim, obs) sqrt(mean((obs - sim/10)^2, na.rm =T)),
           me = function(sim,obs) {mean(obs-sim/10, na.rm = T)}
         ))

rm(list= ls())

# ---- soil_to_param  ----
rm(list= ls())
soil <- slb1_soil
soil <- cbind(soil, hydpar_wessolek_mvg(soil$texture))
str(soil)

soil_layers_materials <- soil_to_param(soil)
soil_layers_materials

rm(list=ls())

# ----- standprop_yearly_to_param ----

param.b90 <- setparam_LWFB90()
dat <- slb1_standprop

years <- 2002:2005
param.new <- standprop_yearly_to_param(dat,
                          param.b90,
                          years)

identical(param.new$maxlai, dat$maxlai[dat$year %in% years])
identical(param.new$height, dat$height[dat$year %in% years])

rm(list = ls())

# ---- extract_layer_output  ----
 # create a data.frame with monthly values
 # identifiers: layer number, yr and mo
 df <- expand.grid(nl = 1:5,
                   yr = 2002:2003,
                   mo = 1:12)
 #value.var
 df$var <- runif(nrow(df), -1,0)

 extract_layer_output(df)

 #more variables
 df$var1 <- runif(nrow(df), 1,2)
 df$var2 <- runif(nrow(df), 2,3)
 # extract specific layers
 extract_layer_output(df,layers = 2:4, sep = "_")
 #extract specific variables
 extract_layer_output(df, layers = 2:4, value.vars = c("var1", "var2"), sep = "_")

# ---- calc_vegperiod ----

calc_vegperiod(out.years = 2001:2010,
               budburst.method = "fixed",
               leaffall.method = "fixed",
               budburstdoy.fixed = floor(runif(10, 120,130)),
               leaffalldoy.fixed = floor(runif(2, 260,280)))


climate <- slb1_meteo

calc_vegperiod(budburst.method = "Menzel",
               leaffall.method = "fixed",
               leaffalldoy.fixed = 280,
               dates = climate$dates,
               tavg = climate$tmean,
               species = "Fagus sylvatica",
               est.prev = 3)

calc_vegperiod(budburst.method = "Menzel",
               leaffall.method = "ETCCDI",
               dates = climate$dates,
               tavg = climate$tmean,
               species = "Quercus robur",
               est.prev = 3)

rm(list = ls())
# ----  calc_globrad ----

calc_globrad(doy = 1:365, sunhours = runif(365, 0, 8),lat = 52.8)

#---- MakeRoots ----

depths <- slb1_soil$lower
roots_beta <- MakeRelRootDens(soilnodes = depths,
                              maxrootdepth = -1,4,
                              beta = 0.97,
                              method = "betamodel")

rootden.table <- data.frame(
  depth = c(-0.02, -0.15, -0.35, -0.5, -0.65,-0.9,-1.1,-1.3,-1.6),
  rootden = c(15, 35, 15, 7.5, 4, 12, 2, 2, 0))

roots_table <- MakeRelRootDens(soilnodes = depths,
                               method = "table",
                               relrootden = rootden.table$rootden,
                               rootdepths = rootden.table$depth)

roots_linear <- MakeRelRootDens(soilnodes = depths,
                                maxrootdepth = -1.4,
                                method = 'linear',
                                relrootden = 0.2)

roots_constant <- MakeRelRootDens(soilnodes = depths,
                                  maxrootdepth = -1.4,
                                  method = 'const',
                                  relrootden = 0.2)

plot(roots_constant, slb1_soil$lower +runif(n=length(slb1_soil$lower), -0.02,0.02),
     type = 's', lwd = 1.5,ylab = "soil depth [m]",xlab = "relative root density",
     xlim = c(0,0.35), col = "red")
lines(roots_linear, slb1_soil$lower,
      type = 's', col = "blue", lwd = 1.5)
lines(roots_table/100, slb1_soil$lower+runif(n=length(slb1_soil$lower), -0.02,0.02),
      type = 's', col = "green", lwd = 1.5)
lines(roots_beta, slb1_soil$lower, type = 's', col = "brown", lwd = 1.5)

legend("bottomright", c("'betamodel'","'table'","'linear'", "'constant'"),seg.len = 1.5,
       pch = NULL, lwd =1.5, col = c("brown", "green", "blue", "red"), bty = "n")

rm(list=ls())

# ----- MakeSeasLAI ----

lai_b90 <- MakeSeasLAI(method = "b90",
                 year = 2001,
                 maxlai = 5,
                 winlaifrac = 0,
                 budburst.doy = 121,
                 leaffall.doy = 280,
                 emerge.dur = 15,
                 leaffall.dur = 30)

lai.doy <- c(1,110,117,135,175,220,250,290,365)
lai.frac <- c(0.1,0.1,0.5,0.7,1.2,1.2,1.0,0.1,0.1)
lai_linear <- MakeSeasLAI(method = "linear",
                 year = 2001,
                 maxlai = 5,
                 lai.doy = lai.doy,
                 lai.frac = lai.frac)

lai_coupmodel <- MakeSeasLAI(method = "Coupmodel",
                             year = 2001,
                             maxlai = 5,
                             winlaifrac = 0.1,
                             budburst.doy = 110,
                             leaffall.doy = 280,
                             shape.optdoy = 180,
                             shape.budburst = 0.5,
                             shape.leaffall = 5)

plot(lai_b90, type = "n", xlab = "doy", ylab = "lai [m²/m²]", ylim = c(0,6))
lines(lai_b90, col ="green",lwd = 2,)
lines(lai_linear, col ="red",lwd = 2)
lines(lai_coupmodel, col ="blue",lwd = 2)

## incorparating between-year variability
years <- 2001:2003
lai <- MakeSeasLAI(method = "Coupmodel",
                             year = years,
                             maxlai = c(4,6,5),
                             budburst.doy = c(100,135,121),
                             leaffall.doy = 280,
                             shape.budburst = c(3,1,0.3),
                             shape.leaffall = 3,
                             shape.optdoy =c(210,180,240) )

dates <- seq.Date(as.Date("2001-01-01"), as.Date("2003-12-31"), by = "day")
plot(dates,lai, col = "green", ylab = "lai [m²/m²]",
     type ="l", xlab = "", lwd = 2)

rm(list = ls())

# ----- prec_corr ---

clim <- slb1_meteo[as.integer(format(slb1_meteo$dates,"%Y")) %in% 2001:2005,]
clim$month <- as.integer(format(clim$dates, "%m"))

prec_meas <- clim$prec
prec_corr_frei <- with(clim,
                       prec_corr(month, tmean, prec, station.exposure = "frei"))
prec_corr_lg <- with(clim,
                       prec_corr(month, tmean, prec, station.exposure = "lg"))
prec_corr_mg <- with(clim,
                     prec_corr(month, tmean, prec, station.exposure = "mg"))
prec_corr_sg <- with(clim,
                     prec_corr(month, tmean, prec, station.exposure = "sg"))

plot(clim$dates, cumsum(prec_corr_frei), type = "l", col = "violet", xlab = "dates", ylab = "cum. precipitation (mm)")
lines(clim$dates, cumsum(prec_corr_lg), col = "blue")
lines(clim$dates, cumsum(prec_corr_mg), col = "green")
lines(clim$dates, cumsum(prec_corr_sg), col = "red")
lines(clim$dates, cumsum(prec_meas))
legend('bottomright', c('frei', "lg", "mg", "sg"),
       col = c("violet", "blue", "green", "red", "black"),
       lty = 1, pch = NULL )

# ---- replace_vec_elements ------
soil_materials <- data.frame(ths = rep(0.4,3), alpha = rep(23.1, 3))

varnms = c("soil_materials.ths3", "soil_materials.ths1", "soil_materials.alpha2")
vals = c(0.999, 0.001, 99)
soil_materials
replace_vecelements(soil_materials, varnms, vals)

x <- setparam_LWFB90()[["pdur"]]
varnms <- c("pdur2", "pdur12")
vals <- c(0,10)
x
replace_vecelements(x, varnms, vals)

# ---- plant linear ----
doys <- c(110,200,250,280)
values <-  c(0,0.8,1,0)
maxdoy <- 365
plot(plant.linear(doys = doys, values = values, maxdoy = 365))


# ---- plant b90 ----

plot(plant.b90(minval = 0,maxval=1,
               doy.incr = 121,incr.dur = 28,
               doy.decr = 280, decr.dur = 50,
               maxdoy = 365))

# ---- plant coupmodel ----
plot(plant.coupmodel(0,5, 121, 200, 280, 0.3, 3, 365))

# ---- setoutput_LWFB90 ----

output <- setoutput_LWFB90()
output

# modify
output[,] <- 0L
output[,3] <- 1L
output["Evap", c("Ann","Mon")] <- 1
output

# open modified
output_new <- setoutput_LWFB90(output)

# open a default output matrix in data editor
output <- setoutput_LWFB90(edit = TRUE)

