!     ******   GETDOM.INC   ******
!     find initial month number and day of month
      TEMP = 0  
!     last DOY of preceding month
      DO 42 J = 1, 12
         IF (DOY .LE. TEMP + DAYMO(J)) THEN
            DOM = DOY - TEMP
            MONTH = J
            GO TO 1043
         ELSE
            TEMP = TEMP + DAYMO(J)
         END IF
   42 CONTINUE
 1043 CONTINUE
