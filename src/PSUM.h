!     ******   PSUM.INC   ******
!        sum flows for precip interval from components
         CALL SUMI(NLAYER, BYFLPI, INFLPI, DSFLPI, TRANPI, DUMM, DUMM, &
            BYFLP, INFLP, DSFLP, TRANP, DUMMY, DUMMY)

!           ^^^^^  ^^^^^  ^^^^^  ^^^^^
         EVAPP = (ISVP + IRVP + SNVP + SLVP) * DTP + TRANP
         FLOWP = SRFLP + BYFLP + DSFLP + GWFLP


