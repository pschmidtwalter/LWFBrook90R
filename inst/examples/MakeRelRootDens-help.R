depths <- c(max(slb1_soil$upper), slb1_soil$lower)
roots_beta <- makeRootden(soilnodes = depths,
                              maxrootdepth = -1,4,
                              beta = 0.97,
                              method = "betamodel")

rootden_table <- data.frame(
  upper = c(0.03,0,-0.02, -0.15, -0.35, -0.5, -0.65,-0.9,-1.1,-1.3),
  lower = c(0,-0.02, -0.15, -0.35, -0.5, -0.65,-0.9,-1.1,-1.3,-1.6),
  rootden = c(10,15, 35, 15, 7.5, 4, 12, 2, 2, 0))

roots_table <- makeRootden(soilnodes = depths,
                               method = "table",
                               rootdat = rootden_table)

roots_linear <- makeRootden(soilnodes = depths,
                                maxrootdepth = -1.4,
                                method = 'linear')

roots_constant <- makeRootden(soilnodes = depths,
                                  maxrootdepth = -1.4,
                                  method = 'const')

plot(roots_constant, slb1_soil$lower +runif(n=length(slb1_soil$lower), -0.02,0.02),
     type = 's', lwd = 1.5,ylab = "soil depth [m]",xlab = "relative root density",
     xlim = c(0,1), col = "red")

lines(roots_linear, slb1_soil$lower,
      type = 's', col = "blue", lwd = 1.5)

lines(roots_beta*10, slb1_soil$lower, type = 's', col = "brown", lwd = 1.5)

lines(roots_table/100, slb1_soil$lower,
      type = 's', col = "green", lwd = 1.5)


legend("bottomright", c("'betamodel'","'table'","'linear'", "'constant'"),seg.len = 1.5,
       pch = NULL, lwd =1.5, col = c("brown", "green", "blue", "red"), bty = "n")
