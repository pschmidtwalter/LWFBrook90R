## Version 0.4.5

* Prof. Brian Ripley's comment to previous v0.4.4: 

 - > A recent check run with setenv MC_CORES 2 shows "Error when sourcing 'multiruns.R': Can not run on 5 cores! Only 2 available."

- **Answer**: The number of CPUs deployed in vignette 'multiruns' was reduced to 2.

### Test environments
* Rocky Linux 4.18.0-305.3.1.el8_4.x86_64 (local): R-4.0.5
* win-builder (http://win-builder.r-project.org/): R-devel / 4.1.2 / 4.0.5
*	Fedora Linux, clang, gfortran (R-hub): R-devel
* Debian Linux, GCC ASAN/UBSAN (R-hub): R-devel
* Ubuntu Linux 20.04.1 LTS, GCC (R-hub): R-4.1.2
* latest Ubuntu (via Github Actions): R-devel / 4.1.2 / 4.0.5
* latest macOS (via Github Actions): R-4.1.2

### R CMD check
There were no errors or warnings.
Some test environments return 1 NOTE because of a known URL-issue with https://doi.org/10.1175/1525-7541(2003)004%3C1276:SOAETS%3E2.0.CO;2
This is a central reference for the package and thus needs to included.




