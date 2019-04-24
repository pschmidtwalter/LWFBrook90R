library(LWFBrook90R)

options.b90 <- setoptions_LWFB90()
param.b90 <- setparam_LWFB90()
soil <- cbind(slb1_soil, with(slb1_soil, hydpar_puh2(sand,silt, clay, bd, c_org)))

soillaymat <- soil_to_param(soil)
param.b90$soil_nodes <- soillaymat$soil_nodes
param.b90$soil_materials <- soillaymat$soil_materials

b90.results.slb1 <- runLWFB90(project.dir = "example_run_b90",
                            options.b90 = options.b90,
                            param.b90 = param.b90,
                            climate = slb1_meteo,
                            output.log = T
                            )

soil <- read.csv("C:/Users/pschmidtwalter/Desktop/soil_slb1.csv", stringsAsFactors = F)
options.b90$imodel <- "CH"
b90.results.slb1 <- runLWFB90(project.dir = "example_run_b90",
                              options.b90 = options.b90,
                              param.b90 = param.b90,
                              climate = slb1_meteo,
                              soil =soil)



names(b90.results.slb1)
#####







# Multirun


# choose the 'Coupmodel' shape option for the annual lai dynamic, with fixed budburst and leaf fall dates:
options.b90$lai.method <- 'Coupmodel'
options.b90$budburst.method <- 'fixed'
options.b90$leaffall.method <- 'fixed'

#set up data.frame with variable leaf area index parameters
n <- 5
vary_parms1 <- data.frame(maxlai = runif(n,4,8),
                          shape.budburst = runif(n, 0.1,1),
                          winlaifrac = runif(n, 0,0.5),
                          budburstdoy = runif(n,100,150),
                          shape.optdoy = runif(n,180,240))


vary_parms2 <- data.frame(maxlai = runif(n,4,8),
                          shape.budburst = runif(n, 0.1,1),
                          winlaifrac = runif(n, 0,0.5),
                          budburstdoy = runif(n,100,150),
                          shape.optdoy =runif(n,180,240))

with(vary_parms1[5,], plot(plant.coupmodel(winlaifrac*maxlai,maxlai,budburstdoy,shape.optdoy,279,shape.budburst,3,365)))


args(brook90r:::plant.coupmodel)

# Make a Multirun-Simulation
b90.multi <- MultiRun.B90(nRuns = n,
                          param_var = vary_parms1,
                          param_const = param.b90,
                          options.b90 = options.b90,
                          soil = soil,
                          climate = meteo_slb1,
                          multirun.dir = "MultiRuns",
                          keep.subdirs = FALSE,
                          singlerun_names = paste0("result.",1:n),
                          cores = 3,
                          output.param.options = F
                          )

b90.multi[1]
