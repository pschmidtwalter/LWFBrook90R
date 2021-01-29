# News for the LWFBrook90R-package

## Version 0.4.1 ()

## Version 0.4.0  (2021-01-15)

Changes:

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


## Version 0.3.4  (2020-08-28)

Changes:

- Adjusted 'table'-method in `MakeRelRootDens()`: The root depth distribution provided in a table is now redistributed to the soil nodes under preservation of the total root mass.
- Input changed for 'soilnodes'-argument of `MakeRelRootDens()`. See `?MakeRelRootDens`.

Bug fixes:

- zero division error causing infinite `relawat` values in `MISCDAY.ASC`-output item.
- 'betamodel'-method of `MakeRelRootDens()` returned the increment of the cumulative root proportion at the soil nodes, instead of the relative root density, which actually is the former value divided by the layer thickness.


## Version 0.3.0 (2020-04-21)

Changes:

- new function `msiterunLWFB90()`
- `runLWFB90()`: model input (`param.b90`, `options.b90`, `standprop_daily`) is appended
	to the return value BEFORE evaluating the `output_fun`-argument. In this way,
	on-the-fly post-processing of model results including model-input is possible now.
- minor bugs fixed


## Version 0.2.0  (2019-06-18)

Changes:

- `output_fun` argument replaces `gof_fun`-argument in `runLWFB90()` for more flexible output.
- minor bugs fixed


## Version 0.1.0  (2019-05-13)

- First beta release: most functionality was taken from [brook90r](https://doi.org/10.5281/zenodo.1433677). However, functions and arguments were renamed, and a lot of functionality was added.
