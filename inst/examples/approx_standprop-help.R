years <- 2002:2004
height_yearly <- c(20.2,20.8,21.3)

# constant 'interpolation'
height_c <- approx_standprop(x_yrs = years,
                             y = height_yearly)

# linear interpolation
height_ini <- 19.1
height_l <- approx_standprop(x_yrs=years,
                             y = height_yearly,
                             y_ini = height_ini,
                             approx.method = 'linear')

# use growthperiod
height_l_gp <- approx_standprop(x_yrs = years,
                                y = height_yearly,
                                y_ini = height_ini,
                                use_growthperiod = TRUE,
                                startdoy = 121,
                                enddoy = 279,
                                approx.method = 'linear')

dates <- seq.Date(from = as.Date(paste0(min(years),"-01-01")),
                  to = as.Date(paste0(max(years),"-12-31")),
                  by = "day")
plot(dates, height_c,
     type = "l", lwd = 2, col = "black",
     ylim = c(19,22), ylab = "height [m]", xlab = "", xpd = TRUE)
lines(dates, height_l,
      col = "blue", lwd = 2)
lines(dates, height_l_gp,
      col = "green", lwd = 2)
legend("topleft", legend = c("'constant'", "'linear'",
                             "'linear', 'use_growthperiod'"),
       col  = c("black", "blue", "green"),  lwd = 2, pch = NULL,
       bty = "n")
