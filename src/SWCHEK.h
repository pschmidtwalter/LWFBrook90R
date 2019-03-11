!     ******   SWCHEK.INC   ******
!     test for SWATI(I) < 0 or > SWATMX(I)
      IF (SWATI(I) .LE. 0.0d0) THEN
         PRINT '(A,E20.5,4(A,I3))', 'You lose!  SWATI=',SWATI(I), &
            ' for layer',I,', year', YEAR, ', month', MONTH, ', day', DOM
         PRINT*, 'Examine output and parameters to try to determine the cause'
         STOP
      ELSEIF (SWATI(I) .GT. SWATMX(I)) THEN
         IF (SWATI(I) .GT. SWATMX(I) + .000010d0) THEN
            PRINT '(4(A,I3))', 'SWATI .GT. SWATMX for layer', I, &
                ', year', YEAR, ', month', MONTH, ', day', DOM
            PRINT*, 'Examine output and parameters to try to determine the cause'
         ELSE
!           rounding error only
            SWATI(I) = SWATMX(I)
         END IF
      END IF

