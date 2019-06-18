
<!-- README.md is generated from README.Rmd. Please edit that file -->

# LWFBrook90R

-----

Run the
[LWF-BROOK90](https://www.lwf.bayern.de/boden-klima/wasserhaushalt/index.php)
hydrological model from within R

## Installation

Before installing the package, the following imported packages need to
be installed from CRAN:

``` r
install.packages("data.table")
install.packages("vegperiod")
install.packages("sirad")
install.packages("foreach")
install.packages("doSNOW")
```

### Recommended installation

It is recommended to download and install the latest stable release from ...

``` r
install.packages("path/to/package/LWFBrook90R_0.1.0.tar.gz", repos = NULL, type = "source")
```

For installing the source package in R under Windows,
[Rtools](https://cran.r-project.org/bin/windows/Rtools/) is required. If
Rtools is not available, install the (.zip) binary
package:

``` r
install.packages("path/to/package/LWFBrook90R_0.1.0.zip", repos = NULL, type = "binary")
```

You can also install the latest stable release directly from GitHub,
using the devtools package:

``` r
if (!requireNamespace("devtools")) {
    install.packages("devtools")
}
devtools::install_url("https://github.com/biometry/rLPJGUESS/releases/download/v1.1.0/rLPJGUESS_1.1.0.tar.gz",
                      dependencies = T, build_vignettes = T)
```

### Development version

Instead of installing the latest stable release, you can also install
the latest development version using the devtools package:

``` r
devtools::install_github(repo = "pschmidtwalter/LWFBrook90R", 
                         dependencies = T, build_vignettes = T)
```

After installation, use `vignette("intro_lwfbrook90r")` to see the
manual.

## Basic usage

Load LWFBrook90R:

``` r
library(LWFBrook90R)
```

To run an example, load meteorological and soil data from the Solling
Beech Experimental site distributed with the package:

``` r
data("slb1_meteo")
data("slb1_soil")
```

Set up lists containing default model control options and model
parameters:

``` r
options.b90 <- setoptions_LWFB90()
param.b90 <- setparam_LWFB90()
```

Set start and end dates in model control options:

``` r
options.b90$startdate <- as.Date("2002-01-01")
options.b90$enddate <- as.Date("2003-12-31")
```

Derive soil hydraulic properties from soil physical properties using a
pedotransfer function:

``` r
soil <- cbind(slb1_soil, hydpar_puh2(clay = slb1_soil$clay,
                                     silt = slb1_soil$silt,
                                     sand = slb1_soil$sand,
                                     bd = slb1_soil$bd,
                                     oc.pct = slb1_soil$c_org))
```

Run LWF-Brook90 with the created model input objects and capture results
in `b90.results.slb1`:

``` r
b90.results.slb1 <- runLWFB90(project.dir = "example_run_b90/",
                              param.b90 = param.b90,
                              options.b90 = options.b90,
                              climate = slb1_meteo,
                              soil = soil)
str(b90.results.slb1, max.level = 1)
```

## Status

### R-Code

The package works as intended and is fully documented. However, there
are some points to be accomplish in the near future:

  - [x] enable use of Clapp-Hornberger hydraulic parameterization in
    addition to the default Mualem-van Genuchten
  - [x] Use of sub-day resolution precipitation interval data.
  - [ ] implement unit tests (currently functionality is tested mostly
    through vignette building)
  - [ ] Run the `check` with Travis.

### Fortran-Code

  - [x] Model output results tested against the output from the original
    ‘b90.exe’ Windows command line Fortran program.
  - [x] Cleaning up `declared but not used` variables
  - [x] Use of sub-day resolution precipitation interval data.
  - [x] Optional writing of model log-file that contains the former
    commandline-feed.
  - [ ] Implementation of an error-routine. Currently, R crashes when
    the interface function `f_brook90` is called with wrong parameters.
    However, this never happens, as `runLWFB90` takes care of correct
    input for `f_brook90`.

## Authors

Paul Schmidt-Walter, Volodymyr Trotsiuk, Klaus Hammel, Martin Kennel,
Tony Federer.

Tony Federer’s original [Brook90 Fortran 77
code](http://www.ecoshift.net/brook/b90doc.html) (Brook90\_v3.1F,
License: CC0) was enhanced by Klaus Hammel and Martin Kennel at Bavarian
State Institute of Forestry (LWF) around the year 2000. Since then,
LWF-BROOK90 is distributed by
[LWF](https://www.lwf.bayern.de/boden-klima/wasserhaushalt/index.php)
upon request as a pre-compiled Fortran command line program together
with in MS Access User Interface. In 2019, Volodymyr Trotsiuk converted
the Fortran 77 code to Fortran 95 and implemented the connection to R.
Paul Schmidt-Walter’s *brook90r* package for LWF-Brook90 input data
generation, model execution and result processing was adapted and
extended to control this interface function.

## License

GPL-3 for all Fortran and R code. brook90r has GPL-3, while LWF-Brook90
was without license until recently. Lothar Zimmermann and Stephan raspe
(LWF), as well as all Fortran contributors agreed to assign GPL-3 to the
Fortran code.
