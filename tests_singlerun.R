library(LWFBrook90R)

# ----- Normal
param.b90 <- setparam_LWFB90()
options.b90 <- setoptions_LWFB90()

# Set start and end Dates for the simulation

options.b90$startdate <- as.Date("2001-01-01")
options.b90$enddate <- as.Date("2001-12-31")
options.b90$prec.corr = F
soil <- cbind(slb1_soil, hydpar_wessolek_mvg(slb1_soil$texture))

# Run LWF-Brook90
b90.result <- runLWFB90(project.dir = "example_run_b90",
                        options.b90 = options.b90,
                        param.b90 = param.b90,
                        climate = slb1_meteo,
                        soil = soil)
b90.result$EVAPANN.ASC$FLOW
# different return values
b90.result <- runLWFB90(project.dir = "example_run_b90",
          options.b90 = options.b90,
          param.b90 = param.b90,
          climate = slb1_meteo,
          soil = soil,
          read.output = T,
          rtrn.input = F,
          run = F)
names(b90.result)

b90.result <- runLWFB90(project.dir = "example_run_b90",
                        options.b90 = options.b90,
                        param.b90 = param.b90,
                        climate = slb1_meteo,
                        soil = soil,
                        read.output = T,
                        output.log = F
                        )


# ---- start and end weird

options.b90$startdate <- as.Date("2018-05-15")
options.b90$enddate <- as.Date("2018-8-31")

b90.result <- runLWFB90(project.dir = "example_run_b90",
                        options.b90 = options.b90,
                        param.b90 = param.b90,
                        climate = slb1_meteo,
                        soil = soil)





