# Set up lists containing model control options and model parameters:
param.b90 <- setparam_LWFB90()
options.b90 <- setoptions_LWFB90()

# Set start and end Dates for the simulation
options.b90$startdate <- as.Date("2003-01-01")
options.b90$enddate <- as.Date("2003-12-31")

# Derive soil hydraulic properties from soil physical properties
# using pedotransfer functions
soil <- cbind(slb1_soil, hydpar_wessolek_tab(slb1_soil$texture))

# Run LWF-Brook90
b90.result <- runLWFB90(options.b90 = options.b90,
                        param.b90 = param.b90,
                        climate = slb1_meteo,
                        soil = soil)

# use a function to be performed on the output:
# aggregate soil water storage down to a specific layer
agg_swat <- function(x, layer) {
  out <- aggregate(swati~yr+doy,
                   x$SWATDAY.ASC,
                   FUN = sum,
                   subset = nl <= layer)
  out[order(out$yr, out$doy),]}

# run model, without returning the original output.
b90.aggswat <- runLWFB90(options.b90 = options.b90,
                         param.b90 = param.b90,
                         climate = slb1_meteo,
                         soil = soil,
                         output_fun = list(swat = agg_swat),
                         rtrn.output = FALSE,
                         layer = 10)  # passed to output_fun
str(b90.aggswat$output_fun$swat)

b90.aggswat
