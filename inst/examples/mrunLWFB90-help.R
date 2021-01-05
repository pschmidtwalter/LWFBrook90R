data("slb1_meteo")
data("slb1_soil")
# Set up lists containing model control options and model parameters:
parms <- setparam_LWFB90()
# choose the 'Coupmodel' shape option for the annual lai dynamic,
# with fixed budburst and leaf fall dates:
opts <- setoptions_LWFB90(startdate = as.Date("2003-06-01"),
                                 enddate = as.Date("2003-06-30"),
                                 lai_method = 'Coupmodel',
                                 budburst_method = 'fixed',
                                 leaffall_method = 'fixed')

# Derive soil hydraulic properties from soil physical properties using pedotransfer functions
soil <- cbind(slb1_soil, hydpar_wessolek_tab(slb1_soil$texture))

#set up data.frame with variable parameters
n <- 2
set.seed(2021)
vary_parms <- data.frame(shp_optdoy = runif(n,180,240),
                         shp_budburst = runif(n, 0.1,1),
                         winlaifrac = runif(n, 0,0.5),
                         budburstdoy = runif(n,100,150),
                         soil_materials.ths3 = runif(n, 0.3,0.5), # ths of material 3
                         maxlai = runif(n,2,7))

# add the soil as soil_nodes and soil materials to param_b90, so ths3 can be looked up
parms[c("soil_nodes", "soil_materials")] <- soil_to_param(soil)

# select outputs
output <- setoutput_LWFB90()

# Make a Multirun-Simulation
b90.multi <- mrunLWFB90(paramvar = vary_parms,
                        param_b90 = parms,
                        options_b90 = opts,
                        climate = slb1_meteo,
                        output = output)
names(b90.multi)

# extract results: EVAPDAY.ASC
evapday <- data.frame(data.table::rbindlist(lapply(b90.multi,
                                                   FUN = function(x) {x[["EVAPDAY.ASC"]]}),
                                            idcol = "srun"))
evapday$dates <- as.Date(paste(evapday$yr, evapday$doy),"%Y %j")

srun_nms <- unique(evapday$srun)

with(evapday[evapday$srun == srun_nms[1], ],
     plot(dates, cumsum(evap), type = "n",
          ylim = c(0,100))
)
for (i in 1:length(b90.multi)){
  with(evapday[evapday$srun == srun_nms[i], ],
       lines(dates, cumsum(evap)))
}


