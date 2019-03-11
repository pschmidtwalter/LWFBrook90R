!     ******   LEAP.INC   ******
!     adjust for leap year
!      IF (YEAR - 4 * INT(YEAR / 4) .EQ. 0) THEN
!         DAYMO(2) = 29
!      ELSE
!         DAYMO(2) = 28
!      END IF

      DAYMO(2) = 29
      IF (YEAR - 4 * INT(YEAR / 4) .EQ. 0) THEN
       IF ((YEAR - 100 * INT(YEAR / 100) .EQ. 0).AND. &
         (YEAR - 400 * INT(YEAR / 400) .NE. 0)) THEN
         DAYMO(2) = 28
       END IF
      ELSE
         DAYMO(2) = 28
      END IF