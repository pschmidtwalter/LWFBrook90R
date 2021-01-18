## Version 0.4.0
* This is a new (and the maintainer's first) submission to CRAN 

## Test environments
* local Ubuntu 20.04.1, R 4.0.3
* Linux Xenial (on travis-ci), R-devel / R 4.0.3 / R 3.6.3
* http://win-builder.r-project.org/ - R-devel / 4.0.3. / 3.6.3)
* rhub (using `rhub::check_for_cran()`)

## R CMD check results

Succeeds with:
0 errors | 0 warnings | 1 note

1. Possibly mis-spelled words in DESCRIPTION:
    LWF (3:9, 12:39)
    SVAT (2:57, 12:27)
    parallelisation (14:9)
    
    **Comment:** I explained SVAT in descriptions of DESCRIPTION file. LWF is an research institute, and I think that 'parallelisation' actually is not mis-spelled
  
2. Found the following (possibly) invalid URLs:
    URL: https://doi.org/10.1175/1525-7541(2003)004%3C1276:SOAETS%3E2.0.CO;2
    From: inst/doc/intro_LWFB90.html
    Status: Error
    Message: libcurl error code 35:
      	schannel: next InitializeSecurityContext failed: SEC_E_ILLEGAL_MESSAGE (0x80090326) - This error usually occurs when a fatal SSL/TLS alert is received (e.g. handshake failed).
      	
    **Comment:** The URL is constructed from DOI and works fine. I would like to keep it in the vignette, as it is important.  



