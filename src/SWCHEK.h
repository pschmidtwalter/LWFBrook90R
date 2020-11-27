!     ******   SWCHEK.INC   ******
!     test for SWATI(I) < 0 or > SWATMX(I)
      IF (SWATI(I) .LE. 0.0d0) THEN
        error = 6
        if ( pr ) call intpr1("STOP: negative soil water content in layer, year, month, day", -1, &
            (/I, YEAR, MONTH, DOM/))
        if ( pr ) call labelpr("Examine output and parameters to determine the cause!", -1)
        go to 999
      ELSEIF (SWATI(I) .GT. SWATMX(I)) THEN
        IF (SWATI(I) .GT. SWATMX(I) + .000010d0) THEN
        error = 7
        if ( pr ) call intpr1("STOP: soil water storage above capacity in layer, year, month, day", -1, &
        (/I, YEAR, MONTH, DOM/))
        if ( pr ) call labelpr("Examine output and parameters to determine the cause!", -1)
        go to 999 ! inserted by Paul Schmidt-Walter, Nov 26, 2020
        ELSE
!       rounding error only
        SWATI(I) = SWATMX(I)
        END IF
      END IF

