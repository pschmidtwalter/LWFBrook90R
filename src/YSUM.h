!     ******   YSUM.INC   ******
!        sum flows for year from components
         CALL SUMI(NLAYER, BYFLYI, INFLYI, DSFLYI, TRANYI, DUMM, DUMM, &
           BYFLY, INFLY, DSFLY, TRANY, DUMMY, DUMMY)
         CALL SUMI(NLAYER, DUMM, DUMM, DUMM, TRANVPI, DUMM, DUMM, &
           DUMMY, DUMMY, DUMMY, TRANVP, DUMMY, DUMMY)

!           ^^^^^  ^^^^^  ^^^^^  ^^^^^
         PRECY = RFALY + SFALY
         STHRY = SFALY - SINTY
         RTHRY = RFALY - RINTY
         RNETY = RTHRY - RSNOY
         EVAPY = IRVPY + ISVPY + SNVPY + SLVPY + TRANY
         EVAPVP = IRVPVP + ISVPVP + SNVPVP + SLVPVP+ TRANVP
         FLOWY = SRFLY + BYFLY + DSFLY + GWFLY

