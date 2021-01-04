options_b90 <- setoptions_LWFB90()
param_b90 <- setparam_LWFB90()

standprop <- make_standprop(options_b90,
                            param_b90,
                            out_yrs = 2002:2004)
plot(standprop$dates, standprop$lai, type = "l")
