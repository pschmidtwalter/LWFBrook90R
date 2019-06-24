library(LWFBrook90R)
library(data.table)

options.b90 <- setoptions_LWFB90()
param.b90 <- setparam_LWFB90()

soil <- cbind(slb1_soil, hydpar_wessolek_mvg(tex.KA5 = slb1_soil$texture))

output <- setoutput_LWFB90()
output[,2:10] <- 1L

b90res <- runLWFB90(project.dir = "example_run_b90/",
                    options.b90 = options.b90,
                    param.b90 = param.b90,
                    climate = slb1_meteo,
                    soil = soil,
                    output = output)
str(b90res, max.level = 1)


with(b90res$model_input, 
     writeParam.in(parameters = param.b90, outmat = output, filename = "in/Param.in"))

climveg <- merge(slb1_meteo, b90res$model_input$standprop_daily, by = "dates")
climveg <- climveg[order(climveg$dates),]
climveg$mesfl <- 0
 
writeClimate.in(climveg = climveg,b90res$model_input$param.b90 )

system2(command = "H:/B90/b90.exe",
        stdout = "output.log",
        invisible = TRUE,
        wait = TRUE
)
exe_res <- brook90r::readOutput.B90("out")
str(exe_res)

var <- c("flow", "evap", "snow", "swat")
exe <- exe_res$budgday.asc
b90 <-b90res$BUDGDAY.ASC
pdf("flow_plots.pdf")

for(i in 1:length(var)) {
 plot(exe[[var[i]]],b90[[toupper(var[i])]])
 abline(0,1, col = "red")
 text(x=quantile(exe[[var[i]]], 0.2),
      y = quantile(b90[[toupper(var[i])]], 0.8),
      labels = paste("sum(b90.exe):", sum(exe[[var[i]]]),"\n",
        "sum(dll):", sum(b90[[toupper(var[i])]]),"\n",
        "diff:",sum(exe[[var[i]]])-sum(b90[[toupper(var[i])]]) ),
      col = "blue")
 title(var[i])
}
dev.off()


exe <- exe_res$abovday.asc
b90 <-b90res$ABOVDAY.ASC
var <- names(exe_res$abovday.asc)[5:13]
pdf("abov_plots.pdf")

for(i in 1:length(var)) {
        plot(exe[[var[i]]],b90[[toupper(var[i])]])
        abline(0,1, col = "red")
        text(x=quantile(exe[[var[i]]], 0.2),
             y = quantile(b90[[toupper(var[i])]], 0.8),
             labels = paste("sum(b90.exe):", sum(exe[[var[i]]]),"\n",
                            "sum(dll):", sum(b90[[toupper(var[i])]]),"\n",
                            "diff:",sum(exe[[var[i]]])-sum(b90[[toupper(var[i])]]) ),
             col = "blue")
        title(var[i])
}
dev.off()

names(exe_res)
exe <- exe_res$evapday.asc
b90 <-b90res$EVAPDAY.ASC
var <- names(exe)[6:15]
pdf("evap_plots.pdf")

for(i in 1:length(var)) {
        plot(exe[[var[i]]],b90[[toupper(var[i])]])
        abline(0,1, col = "red")
        text(x=quantile(exe[[var[i]]], 0.2),
             y = quantile(b90[[toupper(var[i])]], 0.8),
             labels = paste("sum(b90.exe):", sum(exe[[var[i]]]),"\n",
                            "sum(dll):", sum(b90[[toupper(var[i])]]),"\n",
                            "diff:",sum(exe[[var[i]]])-sum(b90[[toupper(var[i])]]) ),
             col = "blue")
        title(var[i])
}
dev.off()

names(exe_res)
exe <- exe_res$flowday.asc
b90 <- b90res$FLOWDAY.ASC
var <- names(exe)[6:13]
pdf("flow_plots.pdf")

for(i in 1:length(var)) {
        plot(exe[[var[i]]],b90[[toupper(var[i])]])
        abline(0,1, col = "red")
        text(x=quantile(exe[[var[i]]], 0.2),
             y = quantile(b90[[toupper(var[i])]], 0.8),
             labels = paste("sum(b90.exe):", sum(exe[[var[i]]]),"\n",
                            "sum(dll):", sum(b90[[toupper(var[i])]]),"\n",
                            "diff:",sum(exe[[var[i]]])-sum(b90[[toupper(var[i])]]) ),
             col = "blue")
        title(var[i])
}
dev.off()

names(exe_res)
exe <- exe_res$miscday.asc
b90 <- b90res$MISCDAY.ASC
var <- names(exe)[5:13]
pdf("misc_plots.pdf")

for(i in 1:length(var)) {
        plot(exe[[var[i]]],b90[[toupper(var[i])]])
        abline(0,1, col = "red")
        text(x=quantile(exe[[var[i]]], 0.2),
             y = quantile(b90[[toupper(var[i])]], 0.8),
             labels = paste("sum(b90.exe):", sum(exe[[var[i]]]),"\n",
                            "sum(dll):", sum(b90[[toupper(var[i])]]),"\n",
                            "diff:",sum(exe[[var[i]]])-sum(b90[[toupper(var[i])]]) ),
             col = "blue")
        title(var[i])
}
dev.off()

