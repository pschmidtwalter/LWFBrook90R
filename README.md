
<!-- README.md is generated from README.Rmd. Please edit that file -->
LWFBrook90R: Run the LWF-BROOK90 hydrological model from within R.
==================================================================

Motivation
==========

In hydrology, many R-packages exist that deal with pre- and post-processing of input data and results of hydrological process models. In addition, many ready-to-use algorithms exist in R providing inverse calibration, sensitivity analysis, and parallelisation techniques. In order to make the vast resources of R directly available to the 1D-SVAT model [LWF-BROOK90](https://www.lwf.bayern.de/boden-klima/wasserhaushalt/index.php), *LWFBrook90R* was developed. The Fortran model code is integrated into the package as dynamic library and compiles when the package is installed.

With one call to the package core function `runLWFB90()`, *LWFBrook90R* features the following functionality:

-   create model input objects from climate driving data, model control options and parameters,
-   execute the LWF-BROOK90 model code,
-   read and return the created output files.

The model control options thereby let the user select different functions for defining aboveground stand dynamics, phenology, and root length density depth distributions. For convenience, a set of functions are available to set up the required lists of model control options and parameters, and to derive soil hydraulic parameters from soil physical properties using pedotransfer-functions. In addition, wrapper-functions for `runLWFB90()` are available to facilitate parallel multi-run simulations and multi-site simulations.

Installation
============

Before installing the *LWFBrook90R* R-package, the following packages have to be installed from CRAN:

``` r
install.packages("data.table")
install.packages("vegperiod")
install.packages("sirad")
install.packages("foreach")
install.packages("doSNOW")
```

Now *LWFBrook90R* can be installed. The package currrently resides in a private repository on Github, but (hopefully) will be available for public on Github and CRAN soon. Then, the latest version can be installed directly from Github.com using the *devtools*-package:

``` r
if (!requireNamespace("devtools")) {
    install.packages("devtools")
  }
devtools::install_github(repo = "pschmidtwalter/LWFBrook90R")
```

For now, the package needs to be installed from a source file:

``` r
install.packages("LWFBrook90R_0.1.0.tar.gz", repos = NULL, type = "source")
```

After installation, use `vignette("intro_lwfbrook90r")` to see the manual.

Basic usage
===========

Load *LWFBrook90R*:

``` r
library(LWFBrook90R)
```

To run an example, load meteorological and soil data from the Solling Beech Experimental site distributed with the package:

``` r
data("slb1_meteo")
data("slb1_soil")
```

Set up lists containing default model control options and model parameters:

``` r
options.b90 <- setoptions_LWFB90()
param.b90 <- setparam_LWFB90()
```

Set start and end dates in model control options:

``` r
options.b90$startdate <- as.Date("2002-01-01")
options.b90$enddate <- as.Date("2003-12-31")
```

Derive soil hydraulic properties from soil physical properties using a pedotransfer function:

``` r
soil <- cbind(slb1_soil, hydpar_puh2(clay = slb1_soil$clay,
                                     silt = slb1_soil$silt,
                                     sand = slb1_soil$sand,
                                     bd = slb1_soil$bd,
                                     oc.pct = slb1_soil$c_org))
```

Run LWF-Brook90 with the created model input objects and capture results in `b90.results.slb1`:

``` r
b90.results.slb1 <- runLWFB90(project.dir = "example_run_b90/",
                              param.b90 = param.b90,
                              options.b90 = options.b90,
                              climate = slb1_meteo,
                              soil = soil)
```

Status
======

R-Code
------

The package works as intended and is fully documented. However, there are some points to be accomplish in the near future:

-   \[x\] enable use of Clapp-Hornberger hydraulic parameterization in addition to the default Mualem-van Genuchten
-   \[x\] Use of sub-day resolution precipitation interval data.
-   \[x\] implement Goodness-of-fit measures with respect to observations that can be returned on top / instead of actual simulation results.
-   \[ \] Run the `check` with Travis.

Fortran-Code
------------

-   \[x\] Use of sub-day resolution precipitation interval data.
-   \[x\] Model output results tested against the output from the original b90.exe commandline tool.
-   \[x\] Cleaning up `declared but not used` variables
-   \[x\] Optional writing of model log-file that contains the former commandline-feed.
-   \[ \] Implementation of an error-routine. Currently, R crashes when the interface function `f_brook90` is called with wrong parameters. However, this never happens, as `runLWFB90` takes care of correct input for `f_brook90`.

Authors
=======

Paul Schmidt-Walter, Volodymyr Trotsiuk, Klaus Hammel, Martin Kennel, Tony Federer.

Tony Federer's original Brook90 Fortran 77 code was enhanced by Klaus Hammel and Martin Kennel around the year 2000 and distributed as pre-compiled LWF-Brook90 Fortran program, together with in MS Access User Interface. In 2019, Volodymyr Trotsiuk converted the Fortran 77 code to Fortran 95 and implemented the connection to R. Paul Schmidt-Walter created the R functions for input data generation, model execution and result processing.

License
=======

GPL-3 for the R-code, as it builds on the former *brook90r*-package. The license for the Fortran code currently is unknown, but the code may be used here with the kind permission of the coyright holders (see the DESCRIPTION-file).
