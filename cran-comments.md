## Version 0.4.0

* This is a new (and the maintainer's first) submission

## Test environments

* Ubuntu Linux 20.04.1, R-4.0.3 [local]

* win-builder (http://win-builder.r-project.org/): R-devel / 4.0.3. / 3.6.3
* Linux Xenial (on travis-ci): R-devel / R-4.0.3 / R-3.6.3
* R-Hub:
  * Debian Linux, GCC ASAN/UBSAN: R-devel
  * Debian Linux, GCC: R-4.0.3
  * Windows Server 2008 R2 SP1, 32/64 bit: R-devel
  * Fedora Linux, clang, gfortran: R-devel
  * Ubuntu Linux 16.04 LTS, GCC: R-4.0.3
  * macOS 10.13.6 High Sierra, CRAN's setup R-4.0.3

## R CMD check

0 errors | 0 warnings | 1 note

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



