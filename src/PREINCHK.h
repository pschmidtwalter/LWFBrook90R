!     ******   PREINCHK.INC   ******   
!     check date order
      IF (YEAR .NE. YY .OR. MONTH .NE. MM .OR. DOM .NE. DD) THEN
         PRINT*, 'PRFILE error - expected', YEAR, MONTH, DOM,'  but got', YY, MM, DD
         STOP
      END IF
      IF (II .NE. N) THEN
         PRINT*, 'PRFILE error - wrong interval value ', YEAR,MONTH,DOM
         STOP
      END IF

