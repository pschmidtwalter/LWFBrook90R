## Version 0.5.2

### Test environments
* R-hub:
  * Ubuntu Linux 20.04.1 LTS, R-release, GCC: R-release (4.2.2)
  * Windows Server 2022, 64 bit: R-devel
  * Fedora Linux, clang, gfortran: R-devel
  * Debian Linux, GCC ASAN/UBSAN: R-devel
* Github Actions:
  * latest Ubuntu: R-devel / R-release / R-old-release 
  * latest macOS: R-release
  * latest Windows: R-release

### R CMD check

0 errors | 0 warnings | <1 notes

There were no errors or warnings.
Some test environments return 1 NOTE because of a known URL-issue with https://doi.org/10.1175/1525-7541(2003)004%3C1276:SOAETS%3E2.0.CO;2
This is a central reference for the package and thus needs to be included.
