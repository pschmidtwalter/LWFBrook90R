library(LWFBrook90R)

setwd('dev/output/')
load('v_input.rda')

out <- r_lwfbrook90(v_siteparam, v_climveg, v_param, v_pdur, v_soil_materials, v_soil_nodes, v_precdat, TRUE)

out$day[1:10,]

out$layer[1:5, ,1]
out$layer[1:5, ,3]
