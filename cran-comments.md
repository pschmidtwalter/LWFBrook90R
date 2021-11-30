## Version 0.4.5

* Prof. Brian Ripley's comment to previous v0.4.4: 

 - > A recent check run with setenv MC_CORES 2 shows "Error when sourcing 'multiruns.R':  Can not run on 5 cores! Only 2 available."

- **Answer**: The number of CPUs deployed in 'multiruns.R' was reduced to 2.

### Test environments
* Local check using image [rhub/debian-gcc-devel-lto](https://hub.docker.com/repository/docker/rhub/debian-gcc-devel-lto)
* Ubuntu Linux 20.04.1, R-4.0.4 [local]
* win-builder (http://win-builder.r-project.org/): R-devel / 4.0.4. / 3.6.3
* Linux Xenial (on travis-ci): R-devel / R-4.0.4 / R-3.6.3

### R CMD check
There were no errors or warnings.
Some test environments return 1 NOTE because of a known URL-issue with https://doi.org/10.1175/1525-7541(2003)004%3C1276:SOAETS%3E2.0.CO;2




