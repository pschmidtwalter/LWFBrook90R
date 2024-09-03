# LWFBrook90R 0.6.0.9000

## Bug fixes

 - fixed problem in parameter replacement multirun-permutations [#65](https://github.com/pschmidtwalter/LWFBrook90R/issues/70)

## Changes:
 
 - added a constant head lower boundary condition: It is now possible to define the depth of a constant ground water table (`param_b90$water_table_depth`). In this way, capillary rise from a water table can be simulated. Default is -9999, meaning no groundwater influence (i.e. unit gradient flow at the bottom of the soil profile). Any depth can be specified, also a depth below the lowest soil layer. If the water table is within the soil profile, soil layers below the water table are saturated with water.
 - added variables 'relawati' (relative plant available water contents of the soil layers) to `layer_output`, and 'snowlq' (liquid water content of snow on the ground) and 'cc' (cold content of snow pack) to `output`. The latter two are important initial variables for continuing a simulation.
 - added 'snowlqini' and 'snowccini' to the list of parameters (`param_b90`). These can now be passed as initial variables to snowlq and cc, for simulation continuation
 - set check.data = F in vegperiod::vegperiod to be compatible with extreme climates

# LWFBrook90R 0.5.3

## Bug fixes

 - fixed R CMD check Warnings due to non-portable use of kind

## Changes

 - Moved input checks for missing values from R to Fortran model code

# LWFBrook90R 0.5.2

## Bug fixes

 - Major bug fixed concerning the calculation of global radiation from sunshine duration hours (see issue [#65](https://github.com/pschmidtwalter/LWFBrook90R/issues/65))
 - small bug concerning aggregation of output variables in `BudgMon` and `BudgAnn` using miscellaneous function `process_output_LWFB90()`
 
## Changes
  
 - default root-growth-period parameter (`rgroper`) was set to 0, to prevent root growth. The parameter defines the period during which the final root length density distribution is reached. The user now needs to actively switch on root growth by choosing a value > 0. It as advised to carefully investigate layer-wise root water uptake (`tran`) when using the root growth module for young stands.

# LWFBrook90R 0.5.1 

## Bug fixes

 - Installation error on single core machines because of a failed vignette build


# LWFBrook90R 0.5.0 (2022-06-13)

## Changes (potentially breaking)

Basic **output** is now at the **precipitation interval** level:

- With `options_b90$prec_interval` > 1, the output of `run_LWFB90()` now contains one row for each day 
and precipitation interval of the simulation.
- Nothing changes with daily precipitation input (`options_b90$prec_interval = 1`).
- Irrespective of the precipitation interval, the unit of water and vapour fluxes is mm/d.
- With respect to precipitation interval output, the list item `daily_output` of the return was renamed to `output`.
- Removed `output`-argument and exported the function `process_outputs_LWFB90()` that does the job.
- New variable added to `output`: `sthr`.
- Removed redundant variable `slvp` from `layer_output` (soil evaporation).
- Removed variable `psiti` from `layer_output` (total potential of soil layer): can be calculated by summing the layers' matrix potententials (`psimi`) and the gravity potentials at the soil layers' midpoints.

## Documentation

- Vignettes split up into smaller articles

## Bug fixes

- Installation error on single core machines because of a failed test with `run_multisite_LWFB90()` (Thanks to Henrik Bengtsson)

# LWFBrook90R 0.4.5 (2021-11-30)

## Changes

- new daily output variables: daily solar (`slrad`), net solar (`solnet`), net longwave (`lngnet`) and net radiation above (`aa`) and below canopy (`asubs`).

## Bug fixes

- `make_rootden()` with `method = 'betamodel'` led to negative 
root density of the top layer when specified `maxrootdepth` was lower than soil depth.
- no more errors when using sub-daily precipitation input
- typo in `hydpar_wessolek_tab()` caused NAs in return for `texture = 'fSms'`
- `soil_to_param()`: some unique soil materials were omitted in the return, if they had multiple occurences in the input `soil`-data.frame.

# LWFBrook90R 0.4.4. (2021-02-24)

- Fixed LTO installation warning on Fedora Linux

# LWFBrook90R 0.4.3 (2021-02-09)

- Minor changes to fulfill CRAN publication requirements

# LWFBrook90R 0.4.2 (2021-02-08)

## Bug fixes

- `run_LWFB90()`: 
  - `options_b90$root_method = 'table'` gave an error in `make_rootden()` because of a wrong argument name.
- Fixed bad URLs in documentation

# LWFBrook90R 0.4.1 (2021-01-02)

## Changes

- Minor changes to fulfill CRAN publication requirements

# LWFBrook90R 0.4.0  (2021-01-15)

## Changes (potentially breaking)

- arguments and functions were renamed for consistency.
- result datasets are now returned directly by the Fortran model code, without 
  the detour of writing .ASC output text files and reading them back into workspace.
- run time output to the console was disabled, only messages are printed when `verbose = T`.  
- `run_LWFB90()` (before `runLWFB90()`): 
  - without specification of an output selection matrix via `output`, two tables 
  including all available output variables are returned: general daily outputs 
  and layer outputs. A proper description of model output variables was added to the help pages.
  - all column names of the .ASC output objects (as selected via `output`) are now in lower case.
  - it is now possible to provide a function as `climate`-argument, instead of a `data.frame`.
  - an execution time limit (elapsed time) can be set to prevent simulations from running too long. 
- `run_multisite_LWFB90()` (before `msiterunLWFB90()`): 
  - It is now possible to provide individual `param_b90` (before `param.b90`) input parameter objects for
  individual climate/soil combinations (i.e. individual locations). The option to 
  provide a list of `options_b90` (before `options.b90`) input objects was disabled.
  - Instead of a list of `climate`-data.frames for a multisite-simulation, a function 
  can be provided for on-the-fly creation of `climate`-`data.frames`. Arguments 
  for the function have to specified via the new `climate_args`-argument of `run_multisite_LWFB90()`. 
  - Names of current `climate`, `soil` and `param_b90` objects are automatically 
   passed from  `run_multisite_LWFB90()` to `run_LWFB90()` and thus become available to `output_fun`.  
- renamed some of the pedotransfer functions and reorganized the documentation for it. See `?ptfs`.
- switched `run_multisite_LWFB90()` and `run_multi_LWFB90()` from superseded packages 'snow' and 'doSNOW' to 'future', 'doFuture' and 'progressr' for parallel computation and progress reporting thereof. Pacifies a check note and is more future-proof (thanks @rnuske).


# LWFBrook90R 0.3.4  (2020-08-28)

## Changes

- Adjusted 'table'-method in `MakeRelRootDens()`: The root depth distribution provided in a table is now redistributed to the soil nodes under preservation of the total root mass.
- Input changed for 'soilnodes'-argument of `MakeRelRootDens()`. See `?MakeRelRootDens`.

## Bug fixes

- zero division error causing infinite `relawat` values in `MISCDAY.ASC`-output item.
- 'betamodel'-method of `MakeRelRootDens()` returned the increment of the cumulative root proportion at the soil nodes, instead of the relative root density, which actually is the former value divided by the layer thickness.


# LWFBrook90R 0.3.0 (2020-04-21)

## Changes

- new function `msiterunLWFB90()`
- `runLWFB90()`: model input (`param.b90`, `options.b90`, `standprop_daily`) is appended
	to the return value *before* evaluating the `output_fun`-argument. In this way,
	on-the-fly post-processing of model results including model-input is possible now.
- minor bugs fixed


# LWFBrook90R 0.2.0  (2019-06-18)

## Changes

- `output_fun` argument replaces `gof_fun`-argument in `runLWFB90()` for more flexible output.
- minor bugs fixed


# LWFBrook90R 0.1.0  (2019-05-13)

- First beta release: most functionality was taken from [brook90r](https://doi.org/10.5281/zenodo.1433677). However, functions and arguments were renamed, and a lot of functionality was added.
