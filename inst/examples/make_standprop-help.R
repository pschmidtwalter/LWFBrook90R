options.b90 <- setoptions_LWFB90()
param.b90 <- setparam_LWFB90()

standprop <- make_standprop(options.b90,
                            param.b90,
                            out.years = 2002:2004)
plot(standprop$dates, standprop$lai, type = "l")
