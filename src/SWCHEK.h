!     ******   SWCHEK.INC   ******
!     test for SWATI(I) < 0 or > SWATMX(I)
      IF (SWATI(I) .LE. 0.0d0) THEN
        error = 6
        if ( pr .EQ. 1 ) then
          call intpr("STOP: negative soil water content in layer, year, month, day", -1, &
            (/ I, YEAR, MONTH, DOM/),4)
          call intpr("Examine output and parameters to determine the cause!", -1,(/ 0/),0)
        end if
        go to 999
      ELSEIF (SWATI(I) .GT. SWATMX(I)) THEN
        IF (SWATI(I) .GT. SWATMX(I) + .000010d0) THEN
        error = 7
          if ( pr .EQ. 1 ) then
            call intpr("STOP: soil water storage above capacity in layer, year, month, day", -1, &
             (/ I, YEAR, MONTH, DOM/),4)
            call intpr("Examine output and parameters to determine the cause!", -1,(/ 0/),0)
          end if
          go to 999 ! inserted by Paul Schmidt-Walter, Nov 26, 2020
        ELSE
!       rounding error only
        SWATI(I) = SWATMX(I)
        END IF
      END IF

