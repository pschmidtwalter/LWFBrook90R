[![Project Status: WIP – Initial development is in progress, but there has not yet been a stable, usable release suitable for the public.](https://www.repostatus.org/badges/latest/wip.svg)](https://www.repostatus.org/#wip)
[![License: GPL v3](https://img.shields.io/badge/License-GPL%20v3-blue.svg)](https://www.gnu.org/licenses/gpl-3.0)

# Intro

This is an R package which make use of the power of brook90 model written in Fortran.


# Changes

The following changes where done compare to orriginal file:

### General code

  - all subroutines and functions were translated to fortran 95
  - variables (`VARDCL.INC`) and constants (`CONSTANT.INC`) not used by model were removed
  - subrotines and functions (`B90.F`) not used by model were removed
  - declaration of `REAL` was changed to `real(kind=8)`
  - constraining double precision, e.g. `0.01` was changed to `0.010d0`
  - all subroutines and functions were checked for consistency
  - The fortran code is `c_binding`.

### Input
  - all inputs are passed directly from `R`. No need to write *climate* and *param* file to *in* directory.
  - the following tables are passed from `R`: *site, climate, param, param_year, materials, soil, output*. Please look at the example bellow for more information.
  
### Output
  - most of the output is now converted to *coma separated file* (`.csv`). Some are still in the original format.
  - output is written to `out/` directory inside the `working directory`, no changes are possible at this stage.
  
# ToDo

There are still couple of things to consider

  - [ ] I had no `PFILE.DAT` file, thus lines `302-308` are commented. Need to be adjusted.
  - [ ] All the output are written to `out/` directory. I was not able to make the output directory custom (due to *c_binding*). I think this is easy to solve issue, but after 2 hours I couldn't due to lack of knowledge.
  - [x] Results need to be tested with original data.
  - [x] There are still many variables which are `declared but not used`, especially in functions and subroutines. This need to be cleaned.
  - [ ] Run the `check` with Travis.
  
  

# Example

This is a simple example how to use the model

```
library(Rbrook90)
library(dplyr)
library(ggplot2)
library(readr)

#' load the data
data('data_rb')

#' Set the options to output all results
output_rb[,c(2:6)] <- as.integer(1)

#' Run the model
r_brook90(
  site = site_rb,
  climate = climate_rb,
  param = param_rb$value,
  paramYear = param_year_rb[,-1],
  materials = materials_rb,
  soil = soil_rb,
  output = output_rb[,-1]
)

#' Explore the results
flowday.df <- read_csv('out/FLOWDAY.csv')

flowday.df %>%
  mutate(date = ISOdate(YR, MO, DA)) %>%
  ggplot()+
  geom_line(aes(date, FLOW))

```
