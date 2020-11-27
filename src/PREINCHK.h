!     ******   PREINCHK.INC   ******   
!     check date order
      IF (YEAR .NE. YY .OR. MONTH .NE. MM .OR. DOM .NE. DD) THEN
         error = 4
         if ( pr ) call intpr1("STOP - inconsitent dates in precipitation input at (yr,month,day)", -1, &
            (/YY, MM, DD/))
         go to 999
      END IF
      IF (II .NE. N) THEN
         error = 5
         call intpr1("STOP: PRFILE error - wrong precipitation interval value at (yr,month,day)!", -1, &
            (/YEAR, MONTH, DOM/))
         !STOP
         go to 999
      END IF

