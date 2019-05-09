library(LWFBrook90R)
options.b90 <- setoptions_LWFB90()
param.b90 <- setparam_LWFB90()
soil <- cbind(slb1_soil, hydpar_wessolek_mvg(tex.KA5 = slb1_soil$texture))

soillaymat <- soil_to_param(soil)
param.b90$soil_nodes <- soillaymat$soil_nodes
param.b90$soil_materials <- soillaymat$soil_materials

param.b90$maxlai = c(4,6)

test <- runLWFB90("testrun",
          options.b90 = options.b90,
          param.b90= param.b90,
          climate = slb1_meteo)

plot(test$model_input$standprop_daily$lai)


n=5
param_var <- data.frame(cintrl = runif(n, 0.1,0.5),
                        soil_materials.ths3 = runif(n,0.3,0.4),
                        soil_materials.ths1 = runif(n,0.4,0.5),
                        soil_materials.alpha2 = runif(n, 99,100),
                        maxlai1 = runif(n,3,5))

results <- mrunLWFB90(paramvar  = param_var,
                      param.b90 = param.b90,
                      options.b90 = options.b90,
                      climate = slb1_meteo,
                      run = T,
                      cores = 2)

names(results[[1]])
