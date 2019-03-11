!     ******   DSUM.INC   ******
!     sum flows for day from components
      CALL SUMI(NLAYER, BYFLDI, INFLDI, DSFLDI, TRANDI, DUMM, DUMM, &
         BYFLD, INFLD, DSFLD, TRAND, DUMMY, DUMMY)
!                                                               
!        ^^^^^  ^^^^^  ^^^^^  ^^^^^
      PRECD = RFALD + SFALD
      STHRD = SFALD - SINTD
      RTHRD = RFALD - RINTD
      RNETD = RTHRD - RSNOD
      EVAPD = IRVPD + ISVPD + SNVPD + SLVPD + TRAND
      FLOWD = SRFLD + BYFLD + DSFLD + GWFLD

