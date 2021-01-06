# This is to compare the output of the windows commandline tool b90.exe to the output
# calculated by this package, using exactly the same input data. The generated data is used in tests.
# Paul Schmidt-Walter, September 24, 2020

library(LWFBrook90R)
library(data.table)
olwd <- getwd()
setwd("H:/R-Packages/LWFBrook90R/data-raw/data_for_testing/b90exe_output")

source("writeparamin.R")
source("writeclimatein.R")

# Set up the input data
data("slb1_soil")
data("slb1_meteo")
opts <- set_optionsLWFB90()
parms <- set_paramLWFB90()

soil <- cbind(slb1_soil, hydpar_wessolek_tab(texture = slb1_soil$texture))

# Create directories for b90.exe
dir.create("in")
dir.create("out")

# Create Param.in file --------------
# produce model input
pkg_input <- run_LWFB90(options_b90 = opts,
                    param_b90 = parms,
                    climate = slb1_meteo,
                    soil = soil,
                    run = F)

#set up output selection matrix
output <- set_outputLWFB90()
output <- rbind(output, matrix(rep(0,times = 3*5), ncol = 5, byrow = T))
row.names(output) <- c("Eval","Budg", "Flow", "Evap", "Abov", "Belo", "Swat", "Psit", "Misc","User")
output[,] <- 0L
output[,1:3] <- 1L
output[c(1,8,10),] <- 0L

#write Param.in
writeParam.in(parameters = pkg_input$param.b90, outmat = output, filename = "in/Param.in")

# Create Climate.in file
climveg <- merge(slb1_meteo, pkg_input$standprop_daily, by = "dates")
climveg <- climveg[order(climveg$dates),]
climveg$mesfl <- 0
writeClimate.in(climveg = climveg,pkg_input$param.b90 )

# Run b90.exe
file.remove(list.files("out", full.names = T))
system2(command = "H:/B90/b90.exe",
        stdout = "",
        invisible = TRUE,
        wait = TRUE
)

#read files
exe_res <- lapply(list.files("out", pattern = ".ASC", full.names = T),
                  data.table::fread,
                  fill = T, stringsAsFactors = FALSE)
names(exe_res) <- list.files("out", pattern = ".ASC")
lapply(exe_res, function(x) setnames(x, names(x), tolower(names(x))))


save(pkg_input, exe_res, file = "exe_output.rda")
setwd(olwd)
rm(list = ls())


