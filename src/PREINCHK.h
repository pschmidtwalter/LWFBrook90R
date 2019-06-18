!     ******   PREINCHK.INC   ******   
!     check date order
      IF (YEAR .NE. YY .OR. MONTH .NE. MM .OR. DOM .NE. DD) THEN
         write(10,*) 'PRFILE error - expected', YEAR, MONTH, DOM,'  but got', YY, MM, DD
         !STOP
         go to 999
      END IF
      IF (II .NE. N) THEN
         write(10,*) 'PRFILE error - wrong interval value ', YEAR,MONTH,DOM
         !STOP
         go to 999
      END IF

