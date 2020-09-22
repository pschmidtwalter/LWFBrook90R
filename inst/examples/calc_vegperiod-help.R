# fixed budburst and leaffall doy
calc_vegperiod(out.years = 2001:2010,
               budburst.method = "fixed",
               leaffall.method = "fixed",
               budburstdoy.fixed = floor(runif(10, 120,130)),
               leaffalldoy.fixed = floor(runif(2, 260,280)))

# dynamic budburst and leaffall using air temperature
data(slb1_meteo)

calc_vegperiod(budburst.method = "Menzel",
               leaffall.method = "fixed",
               leaffalldoy.fixed = 280,
               dates = slb1_meteo$dates,
               tavg = slb1_meteo$tmean,
               species = "Fagus sylvatica",
               est.prev = 3)

calc_vegperiod(budburst.method = "Menzel",
               leaffall.method = "ETCCDI",
               dates = slb1_meteo$dates,
               tavg = slb1_meteo$tmean,
               species = "Quercus robur",
               est.prev = 3)
