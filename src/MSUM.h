!     ******   MSUM.INC   ******
!        sum flows for month from components
         CALL SUMI(NLAYER, BYFLMI, INFLMI, DSFLMI, TRANMI, DUMM, DUMM, &
              BYFLM, INFLM, DSFLM, TRANM, DUMMY, DUMMY)
!                                            
!           ^^^^^  ^^^^^  ^^^^^  ^^^^^
         PRECM = RFALM + SFALM
         STHRM = SFALM - SINTM
         RTHRM = RFALM - RINTM
         RNETM = RTHRM - RSNOM
         EVAPM = IRVPM + ISVPM + SNVPM + SLVPM + TRANM
         FLOWM = SRFLM + BYFLM + DSFLM + GWFLM
