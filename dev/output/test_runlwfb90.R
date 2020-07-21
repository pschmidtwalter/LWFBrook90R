library(LWFBrook90R)
library(data.table)
source("dev/output/runLWFB90.R")

#Set up lists containing model control options and model parameters:
param.b90 <- setparam_LWFB90()
options.b90 <- setoptions_LWFB90()

# Set start and end Dates for the simulation
options.b90$startdate <- as.Date("2003-01-01")
options.b90$enddate <- as.Date("2003-12-31")

# Derive soil hydraulic properties from soil physical properties
# using pedotransfer functions
soil <- cbind(slb1_soil, hydpar_wessolek_mvg(slb1_soil$texture))

# Run LWF-Brook90
b90.result <- runLWFB90(options.b90 = options.b90,
                        param.b90 = param.b90,
                        climate = slb1_meteo,
                        soil = soil,
                        output.log = F)
b90.result$day[, dates := as.Date(paste(yr, mo,da, sep ="-"))]
b90.result$day[, doy := as.integer(format(dates, "%j"))]
b90.result$day[,mesfl:=0]
b90.result$day[,prec := rfal+sfal]


getwd()
simout <- lapply(list.files(".", pattern = "DAY.ASC", full.names = T),
                 data.table::fread,
                 fill = T, stringsAsFactors = FALSE)
names(simout) <- list.files(".", pattern = "DAY.ASC")
simout$PSITDAY.ASC <- NULL

simout$MISCDAY.ASC <- simout$MISCDAY.ASC[, .(YR,MO,DA,DOY, VRFLN,NITS,BALERR)]
names(simout$BUDGDAY.ASC)[13] <- "INTRS"
simout$BUDGDAY.ASC[,INTRS:=NULL]

abovday <- b90.result$day[,tolower(names(simout$ABOVDAY.ASC)), with = F]
budgday <- b90.result$day[,tolower(names(simout$BUDGDAY.ASC)), with = F]
flowday <- b90.result$day[,tolower(names(simout$FLOWDAY.ASC)), with = F]
evapday <- b90.result$day[,tolower(names(simout$EVAPDAY.ASC)), with = F]
miscday <- b90.result$day[,tolower(names(simout$MISCDAY.ASC)), with = F]
evalday <- b90.result$day[,tolower(names(simout$EVALDAY.ASC)), with = F]

names(b90.result$layer)
simout$SWATDAY.ASC[,TEMPERATURE:=NULL]
b90.result$layer[, dates:=as.Date(paste(yr,mo,da,sep="-"))]
b90.result$layer[, doy:=as.integer(format(dates, "%j"))]
swatday <- b90.result$layer[,tolower(names(simout$SWATDAY.ASC)), with = F]
beloday <- b90.result$layer[,tolower(names(simout$BELODAY.ASC)), with = F]

#ABOVDAY
vars <- names(abovday)[5:13]
fromfile <- simout$ABOVDAY.ASC
setnames(fromfile, names(fromfile),tolower(names(fromfile)))
fromfortran <- abovday
pdf("dev/output/abov_plots.pdf")
for(i in 1:length(vars)) {
  plot(fromfile[[vars[i]]],round(fromfortran[[vars[i]]],1))
  abline(0,1, col = "red")
  text(x=min(fromfile[[vars[i]]]),
       y = max(fromfortran[[vars[i]]]),
       labels = paste("sum(fromfile):", sum(fromfile[[vars[i]]]),"\n",
                      "sum(fromfortran):", sum(round(fromfortran[[vars[i]]],1)),"\n",
                      "diff:",sum(fromfile[[vars[i]]])-sum(round(fromfortran[[vars[i]]],1)) ),
       col = "blue",
       adj=c(0,1))
  title(vars[i])
}
dev.off()

#BUDGDAY
vars <- names(budgday)[5:12]
fromfile <- simout$BUDGDAY.ASC
setnames(fromfile, names(fromfile),tolower(names(fromfile)))
fromfortran <- budgday
pdf("dev/output/budg_plots.pdf")
for(i in 1:length(vars)) {
  plot(fromfile[[vars[i]]],round(fromfortran[[vars[i]]],1))
  abline(0,1, col = "red")
  text(x=min(fromfile[[vars[i]]]),
       y = max(fromfortran[[vars[i]]]),
       labels = paste("sum(fromfile):", sum(fromfile[[vars[i]]]),"\n",
                      "sum(fromfortran):", sum(round(fromfortran[[vars[i]]],1)),"\n",
                      "diff:",sum(fromfile[[vars[i]]])-sum(round(fromfortran[[vars[i]]],1)) ),
       col = "blue",
       adj=c(0,1))
  title(vars[i])
}
dev.off()

#FLOWDAY
vars <- names(flowday)[5:13]
fromfile <- simout$FLOWDAY.ASC
setnames(fromfile, names(fromfile),tolower(names(fromfile)))
fromfortran <- flowday
pdf("dev/output/flow_plots.pdf")
for(i in 1:length(vars)) {
  plot(fromfile[[vars[i]]],round(fromfortran[[vars[i]]],1))
  abline(0,1, col = "red")
  text(x=min(fromfile[[vars[i]]]),
       y = max(fromfortran[[vars[i]]]),
       labels = paste("sum(fromfile):", sum(fromfile[[vars[i]]]),"\n",
                      "sum(fromfortran):", sum(round(fromfortran[[vars[i]]],1)),"\n",
                      "diff:",sum(fromfile[[vars[i]]])-sum(round(fromfortran[[vars[i]]],1)) ),
       col = "blue",
       adj=c(0,1))
  title(vars[i])
}
dev.off()

#EVAPDAY
vars <- names(evapday)[5:15]
fromfile <- simout$EVAPDAY.ASC
setnames(fromfile, names(fromfile),tolower(names(fromfile)))
fromfortran <- evapday
pdf("dev/output/evap_plots.pdf")
for(i in 1:length(vars)) {
  plot(fromfile[[vars[i]]],round(fromfortran[[vars[i]]],3))
  abline(0,1, col = "red")
  text(x=min(fromfile[[vars[i]]]),
       y = max(fromfortran[[vars[i]]]),
       labels = paste("sum(fromfile):", sum(fromfile[[vars[i]]]),"\n",
                      "sum(fromfortran):", sum(round(fromfortran[[vars[i]]],3)),"\n",
                      "diff:",sum(fromfile[[vars[i]]])-sum(round(fromfortran[[vars[i]]],3)) ),
       col = "blue",
       adj=c(0,1))
  title(vars[i])
}
dev.off()

#MISCDAY
vars <- names(miscday)[5:7]
fromfile <- simout$MISCDAY.ASC
setnames(fromfile, names(fromfile),tolower(names(fromfile)))
miscday[, c("vrfln","balerr") := list(round(vrfln,1), round(balerr,4))]
fromfortran <- miscday
pdf("dev/output/misc_plots.pdf")
for(i in 1:length(vars)) {
  plot(fromfile[[vars[i]]],fromfortran[[vars[i]]])
  abline(0,1, col = "red")
  text(x=min(fromfile[[vars[i]]]),
       y = max(fromfortran[[vars[i]]]),
       labels = paste("sum(fromfile):", sum(fromfile[[vars[i]]]),"\n",
                      "sum(fromfortran):", sum(fromfortran[[vars[i]]]),"\n",
                      "diff:",sum(fromfile[[vars[i]]])-sum(fromfortran[[vars[i]]]) ),
       col = "blue",
       adj=c(0,1))
  title(vars[i])
}
dev.off()


#EVALDAY
vars <- names(evalday)[5:8]
fromfile <- simout$EVALDAY.ASC
setnames(fromfile, names(fromfile),tolower(names(fromfile)))
fromfortran <- evalday
pdf("dev/output/eval_plots.pdf")
for(i in 1:length(vars)) {
  plot(fromfile[[vars[i]]],round(fromfortran[[vars[i]]],1))
  abline(0,1, col = "red")
  text(x=min(fromfile[[vars[i]]]),
       y = max(fromfortran[[vars[i]]]),
       labels = paste("sum(fromfile):", sum(fromfile[[vars[i]]]),"\n",
                      "sum(fromfortran):", sum(round(fromfortran[[vars[i]]],1)),"\n",
                      "diff:",sum(fromfile[[vars[i]]])-sum(round(fromfortran[[vars[i]]],1)) ),
       col = "blue",
       adj=c(0,1))
  title(vars[i])
}
dev.off()

#SWATDAY
vars <- names(swatday)[6:10]
fromfile <- simout$SWATDAY.ASC
setnames(fromfile, names(fromfile),tolower(names(fromfile)))
swatday[, c("swati","psimi","psiti", "theta", "wetnes") := list(round(swati,1),
                                             round(psimi,1),
                                             round(psiti,1),
                                             round(theta, 3),
                                             round(wetnes, 3))]
fromfortran <- swatday

pdf("dev/output/swat_plots.pdf")
for(i in 1:length(vars)) {
  plot(fromfile[[vars[i]]],fromfortran[[vars[i]]])
  abline(0,1, col = "red")
  text(x=min(fromfile[[vars[i]]]),
       y = max(fromfortran[[vars[i]]]),
       labels = paste("sum(fromfile):", sum(fromfile[[vars[i]]]),"\n",
                      "sum(fromfortran):", sum(fromfortran[[vars[i]]]),"\n",
                      "diff:",sum(fromfile[[vars[i]]])-sum(fromfortran[[vars[i]]]) ),
       col = "blue",
       adj=c(0,1))
  title(vars[i])
}
dev.off()

#BELODAY
vars <- names(beloday)[6:12]
fromfile <- simout$BELODAY.ASC
setnames(fromfile, names(fromfile),tolower(names(fromfile)))
fromfortran <- beloday

pdf("dev/output/belo_plots.pdf")
for(i in 1:length(vars)) {
  plot(fromfile[[vars[i]]],round(fromfortran[[vars[i]]],1))
  abline(0,1, col = "red")
  text(x=min(fromfile[[vars[i]]]),
       y = max(fromfortran[[vars[i]]]),
       labels = paste("sum(fromfile):", sum(fromfile[[vars[i]]]),"\n",
                      "sum(fromfortran):", sum(round(fromfortran[[vars[i]]],1)),"\n",
                      "diff:",sum(fromfile[[vars[i]]])-sum(round(fromfortran[[vars[i]]],1)) ),
       col = "blue",
       adj=c(0,1))
  title(vars[i])
}
dev.off()


