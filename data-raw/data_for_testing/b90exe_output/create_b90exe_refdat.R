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
options.b90 <- setoptions_LWFB90()
param.b90 <- setparam_LWFB90()

soil <- cbind(slb1_soil, hydpar_wessolek_tab(tex.KA5 = slb1_soil$texture))

# Create directories for b90.exe
dir.create("in")
dir.create("out")

# Create Param.in file --------------
# produce model input
b90res <- runLWFB90(options.b90 = options.b90,
                    param.b90 = param.b90,
                    climate = slb1_meteo,
                    soil = soil,
                    run = F)

#set up output selection matrix
output <- setoutput_LWFB90()
output <- rbind(output, matrix(rep(0,times = 3*5), ncol = 5, byrow = T))
row.names(output) <- c("Eval","Budg", "Flow", "Evap", "Abov", "Belo", "Swat", "Psit", "Misc","User")
output[,] <- 0L
output[,3] <- 1L
output[c(1,8,10),] <- 0L

#write Param.in
writeParam.in(parameters = b90res$param.b90, outmat = output, filename = "in/Param.in")

# Create Climate.in file
climveg <- merge(slb1_meteo, b90res$standprop_daily, by = "dates")
climveg <- climveg[order(climveg$dates),]
climveg$mesfl <- 0
writeClimate.in(climveg = climveg,b90res$param.b90 )

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

# create package output
pkg_res <- runLWFB90(options.b90 = options.b90,
                    param.b90 = param.b90,
                    climate = slb1_meteo,
                    soil = soil,
                    output = -1)

nms_dout <- toupper(names(pkg_res$daily_output)[-c(1:4)])

exe_dout <- cbind(
  exe_res$ABOVDAY.ASC[,list(YR,MO,DA,DOY)],
  exe_res$ABOVDAY.ASC[,which(names(exe_res$ABOVDAY.ASC) %in% nms_dout), with = F],
  exe_res$BUDGDAY.ASC[,which(names(exe_res$BUDGDAY.ASC) %in% nms_dout), with = F],
  exe_res$EVAPDAY.ASC[,which(names(exe_res$EVAPDAY.ASC) %in% nms_dout), with = F],
  exe_res$FLOWDAY.ASC[,which(names(exe_res$FLOWDAY.ASC) %in% nms_dout), with = F],
  exe_res$MISCDAY.ASC[,which(names(exe_res$MISCDAY.ASC) %in% nms_dout), with = F])
exe_dout <- exe_dout[,-duplicated(names(exe_dout)), with = F]
exe_lout <- cbind(exe_res$SWATDAY.ASC[,-11, with = F], exe_res$BELODAY.ASC[,-c(1:5), with = F])

setnames(exe_dout, names(exe_dout), tolower(names(exe_dout)))
setnames(exe_lout, names(exe_lout), tolower(names(exe_lout)))

pkg_input <- pkg_res$model_input
save(pkg_input, exe_dout, exe_lout, file = "exe_output.rda")
setwd(olwd)



