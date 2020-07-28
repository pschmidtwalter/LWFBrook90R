# Intraannual courses of leaf area index
lai_b90 <- MakeSeasLAI(method = "b90",
                       year = 2001,
                       maxlai = 5,
                       winlaifrac = 0,
                       budburst.doy = 121,
                       leaffall.doy = 280,
                       emerge.dur = 15,
                       leaffall.dur = 30)

lai.doy <- c(1,110,117,135,175,220,250,290,365)
lai.frac <- c(0.1,0.1,0.5,0.7,1.2,1.2,1.0,0.1,0.1)
lai_linear <- MakeSeasLAI(method = "linear",
                          year = 2001,
                          maxlai = 5,
                          lai.doy = lai.doy,
                          lai.frac = lai.frac)

lai_coupmodel <- MakeSeasLAI(method = "Coupmodel",
                             year = 2001,
                             maxlai = 5,
                             winlaifrac = 0.1,
                             budburst.doy = 110,
                             leaffall.doy = 280,
                             shape.optdoy = 180,
                             shape.budburst = 0.5,
                             shape.leaffall = 5)

plot(lai_b90, type = "n", xlab = "doy", ylab = "lai [m²/m²]", ylim = c(0,6))
lines(lai_b90, col ="green",lwd = 2,)
lines(lai_linear, col ="red",lwd = 2)
lines(lai_coupmodel, col ="blue",lwd = 2)

# incorparating between-year variability
years <- 2001:2003
lai <- MakeSeasLAI(method = "Coupmodel",
                   year = years,
                   maxlai = c(4,6,5),
                   budburst.doy = c(100,135,121),
                   leaffall.doy = 280,
                   shape.budburst = c(3,1,0.3),
                   shape.leaffall = 3,
                   shape.optdoy =c(210,180,240) )

dates <- seq.Date(as.Date("2001-01-01"), as.Date("2003-12-31"), by = "day")
plot(dates,lai, col = "green", ylab = "lai [m²/m²]",
     type ="l", xlab = "", lwd = 2)
