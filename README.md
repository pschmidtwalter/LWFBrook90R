
<!-- README.md is generated from README.Rmd. Please edit that file -->

# LWFBrook90R

Run the
[LWF-BROOK90](https://www.lwf.bayern.de/boden-klima/wasserhaushalt/index.php)
hydrological model in R.

## Installation

It is recommended to use the latest **stable version**, which can be
found at <https://github.com/pschmidtwalter/LWFBrook90R/releases>. If
`LWFBrook90R_*.tar.gz` / `LWFBrook90R_*.zip` are available for the
latest release, you can download it and install it on Linux / Windows in
the usual way.

Otherwise (or if you want to install from source) you can install a
tagged version directly from the Github repo. On Windows this requires
[Rtools](https://cran.r-project.org/bin/windows/Rtools/). The tag of the
latest release (e.g. `v0.3.4`) can also be found on the
[releases-page](https://github.com/pschmidtwalter/LWFBrook90R/releases).

``` r
if(!require("remotes")) install.packages("remotes")
remotes::install_github(repo="pschmidtwalter/LWFBrook90R", build_vignettes=TRUE,
                        ref="v0.3.4") 
```

<br />

The current **development version** can be installed in the same way

``` r
if(!require("remotes")) install.packages("remotes")
remotes::install_github(repo="pschmidtwalter/LWFBrook90R", build_vignettes=TRUE) 
```

After installation, take a look at the vignette with
`vignette("intro_lwfbrook90r")`.

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
  - [ ] implement unit tests (functionality is currently tested through
    vignette and examples)
  - [ ] Run the `check` with Travis.

### Fortran-Code

  - [x] Model output results tested against the output from the original
    ‘b90.exe’ Windows command line Fortran program.
  - [x] Cleaning up `declared but not used` variables
  - [x] Use of sub-day resolution precipitation interval data.
  - [x] Optional writing of model log-file that contains the former
    commandline-feed.
  - [x] Implementation of an error-routine. All STOP/EXIT commands
    removed from Fortran code.
  - [x] Return model output directly back to R, without writing text
    files.

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
with an MS Access User Interface. In 2019, Volodymyr Trotsiuk converted
the Fortran 77 code to Fortran 95 and implemented the connection to R.
Paul Schmidt-Walter’s *brook90r* package for LWF-Brook90 input data
generation, model execution and result processing was adapted and
extended to control this interface function.

## License

GPL-3 for all Fortran and R code. brook90r has GPL-3, while LWF-Brook90
was without license until recently. Lothar Zimmermann and Stephan Raspe
(LWF), as well as all Fortran contributors agreed to assign GPL-3 to the
Fortran code.
