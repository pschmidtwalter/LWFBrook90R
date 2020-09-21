News for the LWFBrook90R-package
================

# LWFBrook90R v0.?.?
Release date: ???

Changes:

- switched `msiterunLWFB90()` and `mrunLWFB90()` from superseded packages `snow` and `doSNOW` to `future`, `doFuture` and `progressr` for parallel computation and progress reporting thereof. Pacifies a check note and is more future-proof.

----

# LWFBrook90R v0.3.4
Release date: 2020-08-28

Changes:

- Adjusted 'table'-method in `MakeRelRootDens`: The root depth distribution provided in a table is now redistributed to the soil nodes under preservation of the total root mass.
- Input changed for 'soilnodes'-argument of `MakeRelRootDens`. See `?MakeRelRootDens`.

Bug fixes:

- zero division error causing infinite relawat values in `MISCDAY.ASC`-output item.
- 'betamodel'-method of `MakeRelRootDens` returned the increment of the cumulative root proportion at the soil nodes, instead of the relative root density, which actually is the former value divided by the layer thickness.

----

# LWFBrook90R v0.3.0
Release date: 2020-04-21

Changes:

- new function msiterunLWFB90()
- runLWFB90(): model input (param.b90, option.b90, standprop_daily) is appended
	to the return value BEFORE evaluating the output_fun-argument. In this way,
	on-the-fly post-processing of model results including model-input is possible now.
- minor bugs fixed

----

# LWFBrook90R v0.2.0
Release date: 2019-06-18

Changes:

- 'output_fun' argument replaces 'gof_fun'-argument in runLWFB90() for more flexible output.
- minor bugs fixed

----

# LWFBrook90R v0.1.0
Release date: 2019-05-13

- First beta release: most functionality was taken from [brook90r](https://doi.org/10.5281/zenodo.1433677). However, functions and arguments were renamed, and a lot of functionality was added.
