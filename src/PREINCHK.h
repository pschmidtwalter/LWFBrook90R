!     ******   PREINCHK.INC   ******
!     check date order
      IF (YEAR .NE. YY .OR. MONTH .NE. MM .OR. DOM .NE. DD) THEN
         error = 4
         if ( pr ) then
           call intpr("STOP - inconsitent dates in precipitation input - expected (yr,month,day)", -1, &
              (/YEAR, MONTH, DOM/),3)
           call intpr("but got", -1, (/YY, MM, DD/),3)
         end if
         go to 999
      END IF
      IF (II .NE. N) THEN
         error = 5
         if (pr) then
          call intpr("STOP: PRFILE error - wrong precipitation interval value at (yr,month,day)!", -1, &
             (/YY, MM, DD/),3)
         !STOP
         end if
         go to 999
      END IF
