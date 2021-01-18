<!-- badges: start -->
[![Project Status: Active – The project has reached a stable, usable state and is being actively developed.](http://www.repostatus.org/badges/latest/active.svg)](http://www.repostatus.org/#active)
[![Travis build status](https://travis-ci.com/pschmidtwalter/LWFBrook90R.svg?branch=master)](https://travis-ci.com/pschmidtwalter/LWFBrook90R)
<!-- badges: end -->
  
# LWFBrook90R

`LWFBroo90R` provides an implementation of the Soil Vegetation Atmosphere
Transport (SVAT) model
[LWF-BROOK90](https://www.lwf.bayern.de/boden-klima/wasserhaushalt/index.php)
(Hammel & Kennel, 2001) written in Fortran. The model simulates daily
transpiration, interception, soil and snow evaporation, streamflow and soil
water fluxes through a soil profile covered with vegetation. A set of high-level
functions for model set up, execution and parallelization provide easy access to
plot-level SVAT simulations, as well as multi-run and large-scale applications.

## Installation

### Released version
The package will hopefully be available on [CRAN](https://CRAN.R-project.org)
soon. Then, you can install the released version with:

``` r
install.packages("LWFBrook90R")
```

### Development release
For now it is recommended to use the latest stable version, which can be found
at <https://github.com/pschmidtwalter/LWFBrook90R/releases>. If
`LWFBrook90R_*.tar.gz` / `LWFBrook90R_*.zip` are available for the latest
release, you can download it and install it on Linux / Windows in the usual way.

Otherwise (or if you want to install from source) you can install a tagged
version directly from the Github repo. On Windows this requires
[Rtools](https://cran.r-project.org/bin/windows/Rtools/). The tag of the latest
release (e.g. `v0.4.0`) can also be found on the
[releases-page](https://github.com/pschmidtwalter/LWFBrook90R/releases).

``` r
if(!require("remotes")) install.packages("remotes")
remotes::install_github(repo="pschmidtwalter/LWFBrook90R", build_vignettes=TRUE,
                        ref="v0.4.0") 
```

### Development version

The current development version can be installed in the same way

``` r
if(!require("remotes")) install.packages("remotes")
remotes::install_github(repo="pschmidtwalter/LWFBrook90R", build_vignettes=TRUE) 
```

## Usage

Below is basic example. For more complex examples take a look at the
packages vignettes with browseVignettes("LWFBrook90R")`.

The main function `run_LWFB90()` creates the model input from model control
options, parameters, climate and soil data and returns the selected simulation
results.

``` r
# load package and sample data
library(LWFBrook90R)
data(slb1_meteo, slb1_soil)

# set up default model control options and parameters
opts <- set_optionsLWFB90()
parms <- set_paramLWFB90()

# Derive soil hydraulic properties from soil physical properties 
# using a pedotransfer function: 
soil <- cbind(slb1_soil, hydpar_puh2(clay = slb1_soil$clay,
                                     silt = slb1_soil$silt,
                                     sand = slb1_soil$sand,
                                     bd = slb1_soil$bd,
                                     oc.pct = slb1_soil$c_org))

# default output selection
output <- set_outputLWFB90()

# run the model and capture results
lwfb90_res <- run_LWFB90(options_b90 = opts,
                               param_b90 = parms,
                               climate = slb1_meteo,
                               soil = soil)

# View structure of result list object
str(lwfb90_res, max.level = 1)
```

## Citation

Schmidt-Walter, P., Trotsiuk, V., Meusburger, K., Zacios, M., Meesenburg, H.
(2020): Advancing simulations of water fluxes, soil moisture and drought stress
by using the LWF-Brook90 hydrological model in R. Agr. For. Met. 291, 108023.
https://doi.org/10.1016/j.agrformet.2020.108023

## Authors and their contributions

Paul Schmidt-Walter, Volodymyr Trotsiuk, Klaus Hammel, Martin Kennel,
Tony Federer.

Tony Federer’s original [Brook90](http://www.ecoshift.net/brook/b90doc.html)
Fortran 77 code (Brook90\_v3.1F, License: CC0) was enhanced by Klaus Hammel and
Martin Kennel at Bavarian State Institute of Forestry (LWF) around the year
2000. Since then, LWF-BROOK90 is distributed by
[LWF](https://www.lwf.bayern.de/boden-klima/wasserhaushalt/index.php) upon
request as a pre-compiled Fortran command line program together with an MS
Access User Interface. In 2019, Volodymyr Trotsiuk converted the Fortran 77 code
to Fortran 95 and implemented the connection to R. Paul Schmidt-Walter’s
brook90r (https://doi.org/10.5281/zenodo.1433677) package for LWF-Brook90 input
data generation, model execution and result processing was adapted and extended
to control this interface function.

## License

GPL-3 for all Fortran and R code. 'brook90r' has GPL-3, while LWF-Brook90
was without license until recently. Lothar Zimmermann and Stephan Raspe
(LWF), as well as all Fortran contributors agreed to assign GPL-3 to the
Fortran code.

## References

Federer C.A. (2002): BROOK 90: A simulation model for evaporation, soil water, and streamflow.
http://www.ecoshift.net/brook/brook90.htm

Federer C.A., Vörösmarty, C., Fekete, B. (2003): Sensitivity of Annual Evaporation to Soil and Root Properties in Two Models of Contrasting Complexity. J. Hydrometeorol. 4, 1276–1290. https://doi.org/10.1029/96WR00801

Hammel, K., Kennel, M. (2001): Charakterisierung und Analyse der Wasserverfügbarkeit und des Wasserhaushalts von Waldstandorten in Bayern mit dem Simulationsmodell BROOK90. Forstliche Forschungsberichte München 185.


