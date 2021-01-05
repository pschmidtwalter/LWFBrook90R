# Intraannual courses of leaf area index
lai_b90 <- makeSeasLAI(method = "b90",
                       year = 2001,
                       maxlai = 5,
                       winlaifrac = 0,
                       budburst_doy = 121,
                       leaffall_doy = 280,
                       emerge_dur = 15,
                       leaffall_dur = 30)

lai_doy <- c(1,110,117,135,175,220,250,290,365)
lai_frac <- c(0.1,0.1,0.5,0.7,1.2,1.2,1.0,0.1,0.1)
lai_linear <- makeSeasLAI(method = "linear",
                          year = 2001,
                          maxlai = 5,
                          lai_doy = lai_doy,
                          lai_frac = lai_frac)

lai_coupmodel <- makeSeasLAI(method = "Coupmodel",
                             year = 2001,
                             maxlai = 5,
                             winlaifrac = 0.1,
                             budburst_doy = 110,
                             leaffall_doy = 280,
                             shp_optdoy = 180,
                             shp_budburst = 0.5,
                             shp_leaffall = 5)

plot(lai_b90, type = "n", xlab = "doy", ylab = "lai [m²/m²]", ylim = c(0,6))
lines(lai_b90, col ="green",lwd = 2,)
lines(lai_linear, col ="red",lwd = 2)
lines(lai_coupmodel, col ="blue",lwd = 2)

# incorparating between-year variability
years <- 2001:2003
lai <- makeSeasLAI(method = "Coupmodel",
                   year = years,
                   maxlai = c(4,6,5),
                   budburst_doy = c(100,135,121),
                   leaffall_doy = 280,
                   shp_budburst = c(3,1,0.3),
                   shp_leaffall = 3,
                   shp_optdoy =c(210,180,240) )

dates <- seq.Date(as.Date("2001-01-01"), as.Date("2003-12-31"), by = "day")
plot(dates,lai, col = "green", ylab = "lai [m²/m²]",
     type ="l", xlab = "", lwd = 2)
