## Version 0.4.3

* Third resubmission.
* Uwe Ligge's (CRAN) comment:

 - > Found the following (possibly) invalid URLs:
    URL: http://doi.org/10.5281/zenodo.1433677 (moved to https://doi.org/10.5281/zenodo.1433677)
      From: README.md
      Status: 200
      Message: OK 
 - **Answer**: I am wondering how this could happen! I fixed it. Thank you very much!

### Test environments

* Ubuntu Linux 20.04.1, R-4.0.3 [local]

* win-builder (http://win-builder.r-project.org/): R-devel / 4.0.3. / 3.6.3
* Linux Xenial (on travis-ci): R-devel / R-4.0.3 / R-3.6.3

### R CMD check

There were no errors or warnings.

## Version 0.4.2

* This is the 2nd resubmission. Apart from required changes induced by G. Seyers comments below, a bug was fixed, the title was slightly modified, the description field in DESCRIPTION was extended for more information, and the funding institutions were listed in Authors@R field, following advice of R. Nuske. 
* Gregor Seyer's (CRAN) comments led to the following changes:

  - > Found the following (possibly) invalid URLs: 
    > URL: https://doi.org/10.1175/1525-7541(2003)004
    > From: README.md
    > Status: 404
    > Message: Not Found
  - **Answer**: *The URL should now be encoded properly using `URLencode()` and works fine. However, win-builder still raises a NOTE (see R CMD check), and I don't know how to fix it. A web search revealed another R developer also had problems with a DOI linking to another AMS Journal (https://stackoverflow.com/questions/66078248/invalid-url-in-cran-checks), pointing to a server problem. Can it be ignored?*

  - > URL: https://doi.org/10.1175/1525-7541(2003)004<1276:SOAETS>2.0.CO;2
    > From: inst/doc/intro_LWFB90.html
    > Status: Error
    > Message: libcurl error code 35:
            schannel: next InitializeSecurityContext failed: SEC_E_ILLEGAL_MESSAGE (0x80090326) - This error usually occurs when a fatal SSL/TLS alert is received (e.g. handshake failed).
  - **Answer**: *The DOI (as a "service" to the reader) was removed from the vignette, because it was not encoded correctly when rendered from 'refs.bib' to an URL.*

  - > Found the following (possibly) invalid DOIs:
    > DOI: 10.1175/1525-7541(2003)004%3C1276:SOAETS%3E2.0.CO;2
    > From: DESCRIPTION
    > Status: libcurl error code 35:
          schannel: next InitializeSecurityContext failed: SEC_E_ILLEGAL_MESSAGE (0x80090326) - This error usually occurs when a fatal SSL/TLS alert is received (e.g. handshake failed).
    > Message: Error
  - **Answer**: The DOI is encoded correctly and works fine. Can it be ignored (see above)?

### Test environments

* Ubuntu Linux 20.04.1, R-4.0.3 [local]

* win-builder (http://win-builder.r-project.org/): R-devel / 4.0.3. / 3.6.3
* Linux Xenial (on travis-ci): R-devel / R-4.0.3 / R-3.6.3

### R CMD check

There were no errors or warnings.

Some test environments return 1 NOTE:

- Possibly mis-spelled words in DESCRIPTION:
  Evapotranspiration (2:17)
  Federer (13:40)
  Hammel (12:67)
  LWF (3:15, 12:39)
  SVAT (2:63, 12:27, 17:39)
  al (13:51)
  et (13:48)
  parallelization (16:67)
  streamflow (15:5)

- Found the following (possibly) invalid URLs:
  URL: https://doi.org/10.1175/1525-7541(2003)004%3C1276:SOAETS%3E2.0.CO;2
    From: README.md
    Status: Error
    Message: libcurl error code 35:
      	schannel: next InitializeSecurityContext failed: SEC_E_ILLEGAL_MESSAGE (0x80090326) - This error usually occurs when a fatal SSL/TLS alert is received (e.g. handshake failed).

- Found the following (possibly) invalid DOIs:
  DOI: 10.1175/1525-7541(2003)004%3C1276:SOAETS%3E2.0.CO;2
    From: DESCRIPTION
    Status: libcurl error code 35:
    	schannel: next InitializeSecurityContext failed: SEC_E_ILLEGAL_MESSAGE (0x80090326) - This error usually occurs when a fatal SSL/TLS alert is received (e.g. handshake failed).
    Message: Error

## Version 0.4.1

* This is a resubmission.
* CRAN's comments led to the following changes:

  - > *If there are references describing the methods in your package, please add these in the description field of your DESCRIPTION file.*
  - The description field was adjusted, references were included. However, the DOI results in a new NOTE. See section 'R CMD check'.
  - > *Please unwrap the examples if they are executable in < 5 sec, or replace `dontrun{}` with `donttest{}`.*
  - `dontrun{}` was replaced by `donttest{}` in examples.
  - > *Please always make sure to reset to user's options(), working directory or par() after you changed it in examples and vignettes and demos.*
  - After manipulations, the mentioned objects were reset to the user's previous state in various places.
  - > *Please do not modify the user's global environment or the user's home filespace by deleting objects*
  - The `rm(list = ls())` commands in the test files were removed. I thought it was necessary to clean up the workspace after testing.

### Test environments

* Ubuntu Linux 20.04.1, R-4.0.3 [local]

* win-builder (http://win-builder.r-project.org/): R-devel / 4.0.3. / 3.6.3
* Linux Xenial (on travis-ci): R-devel / R-4.0.3 / R-3.6.3

### R CMD check

There were no errors or warnings.

Some test environments return 1 NOTE:

1. **NEW** Found the following (possibly) invalid DOIs:
  DOI: 10.1175/1525-7541(2003)004%3C1276:SOAETS%3E2.0.CO;2
    From: DESCRIPTION
    Status: libcurl error code 35:
    	schannel: next InitializeSecurityContext failed: SEC_E_ILLEGAL_MESSAGE (0x80090326) - This error usually occurs when a fatal SSL/TLS alert is received (e.g. handshake failed).
    Message: Error
    
  **Comment:** DOI reference used in description field is the result of `URLencode('doi:10.1175/1525-7541(2003)004<1276:SOAETS>2.0.CO;2')` The raw DOI is fine when looked up at https://doi.org/, but also gives a check NOTE, when included as is in description.

2. Possibly mis-spelled words in DESCRIPTION:
    Federer (13:40)
    Hammel (12:67)
    LWF (3:9, 12:39)
    SVAT (2:57, 12:27, 15:39)
    al (13:51)
    et (13:48)
    parallelization (14:67)

3. Found the following (possibly) invalid URLs:
    URL: https://doi.org/10.1175/1525-7541(2003)004%3C1276:SOAETS%3E2.0.CO;2
    From: inst/doc/intro_LWFB90.html
    Status: Error
    Message: libcurl error code 35:
      	schannel: next InitializeSecurityContext failed: SEC_E_ILLEGAL_MESSAGE (0x80090326) - This error usually occurs when a fatal SSL/TLS alert is received (e.g. handshake failed).
  
## Version 0.4.0

* This is a new (and the maintainer's first) submission

### Test environments

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
  
### R CMD check
There were no ERRORs or WARNINGs

Some test environments return 1 NOTE:

* checking CRAN incoming feasibility ... NOTE
Maintainer: 'Paul Schmidt-Walter <paulsw@posteo.de>'

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



