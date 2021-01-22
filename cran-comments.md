## Version 0.4.0

* This is a new (and the maintainer's first) submission

## Test environments

* Ubuntu Linux 20.04.1, R-4.0.3 [local]

* win-builder (http://win-builder.r-project.org/): R-devel / 4.0.3. / 3.6.3
* Linux Xenial (on travis-ci): R-devel / R-4.0.3 / R-3.6.3
* R-Hub:
  * Debian Linux, R-devel, GCC ASAN/UBSAN checkArgs=--use-valgrind
  * Debian Linux, R-devel, GCC ASAN/UBSAN
  * Fedora Linux, R-devel, clang, gfortran
  * Ubuntu Linux 16.04 LTS, R-4.0.3, GCC
  * macOS 10.13.6 High Sierra, R-4.0.3, CRAN's setup
  * Windows Server 2008 R2 SP1, R-devel, 32/64 bit
  
## R CMD check
There were no ERRORs or WARNINGs

Some test environments return 1 NOTE:

* checking CRAN incoming feasibility ... NOTE
Maintainer: 'Paul Schmidt-Walter <paul.schmidt-walter@nw-fva.de>'

New submission 

1. Possibly mis-spelled words in DESCRIPTION:
    LWF (3:9, 12:39),
    SVAT (2:57, 12:27),
    parallelization (14:9)
    
    **Comment:** I explained 'SVAT' in description of DESCRIPTION file. 'LWF' is a research institute, and I think that 'parallelization' actually is not mis-spelled.
  
2. Found the following (possibly) invalid URLs:
    URL: https://doi.org/10.1175/1525-7541(2003)004%3C1276:SOAETS%3E2.0.CO;2
    From: inst/doc/intro_LWFB90.html
    Status: Error
    Message: libcurl error code 35:
      	schannel: next InitializeSecurityContext failed: SEC_E_ILLEGAL_MESSAGE (0x80090326) - This error usually occurs when a fatal SSL/TLS alert is received (e.g. handshake failed).
      	
    **Comment:** The URL is constructed from DOI and works fine. 



