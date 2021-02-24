## Version 0.4.4

* Prof. Brian Ripley's comments to previous v0.4.3: [Additional **LTO** issues](https://www.stats.ox.ac.uk/pub/bdr/LTO/LWFBrook90R.out)

  - > Warning: Array ‘par’ at (1) is larger than limit set by ‘-fmax-stack-var-size=’, moved from stack to static storage. This makes the procedure unsafe when called recursively, or concurrently from multiple threads. Consider using ‘-frecursive’, or increase the ‘-fmax-stack-var-size=’ limit, or change the code to use an ALLOCATABLE array.
- **Answer**: The array was made allocatable.
 
  - > skeleton.c:7:6: warning: type of ‘s_brook90_f_’ does not match original declaration [-Wlto-type-mismatch]
  - > md_brook90.f95:33:22: note: ‘s_brook90_f’ was previously declared here 33 | subroutine s_brook90_f( siteparam, climveg, param,...
- **Answer**: Fixed.

### Test environments
* Local check using image [rhub/debian-gcc-devel-lto](https://hub.docker.com/repository/docker/rhub/debian-gcc-devel-lto)
* Ubuntu Linux 20.04.1, R-4.0.4 [local]
* win-builder (http://win-builder.r-project.org/): R-devel / 4.0.4. / 3.6.3
* Linux Xenial (on travis-ci): R-devel / R-4.0.4 / R-3.6.3

### R CMD check
There were no errors or warnings.
Some test environments return 1 NOTE because of a known URL-issue with https://doi.org/10.1175/1525-7541(2003)004%3C1276:SOAETS%3E2.0.CO;2




