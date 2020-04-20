library(LWFBrook90R)

setwd('dev/output/')
load('v_input.rda')


v_output[,] <- 0L
v_output[c('Abov', 'Budg', 'Evap', 'Flow', 'Misc', 'Swat', 'Belo'), 'Day'] <- 1L

out <- r_lwfbrook90(v_siteparam, v_climveg, v_param, v_pdur, v_soil_materials, v_soil_nodes, v_precdat, v_output, v_output_log)

out$day[1:10,]

out$layer[1:5, ,1]
out$layer[1:5, ,3]
