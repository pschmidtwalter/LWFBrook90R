!     ******   DACCUM.INC   ******
!        accumulate above ground flows over day
!        zeroed by ZDAY.INC
         ISVPD = ISVPD + ISVP * DTP
         IRVPD = IRVPD + IRVP * DTP
         SNVPD = SNVPD + SNVP * DTP
!         SLVPD = SLVPD + SLVP * DTP
         SFALD = SFALD + SFAL * DTP
         RFALD = RFALD + RFAL * DTP
         SINTD = SINTD + SINT * DTP
         RINTD = RINTD + RINT * DTP
         RSNOD = RSNOD + RSNO * DTP
         SMLTD = SMLTD + SMLT * DTP
         MESFLD = MESFLD + MESFLP * DTP
         PTRAND = PTRAND + PTRAN * DTP
         PINTD = PINTD + PINT * DTP
!        accumulate below ground flows over day
         CALL ACCUMI(NLAYER, VRFLPI, INFLPI, BYFLPI, DSFLPI, NTFLPI, &
         VRFLDI, INFLDI, BYFLDI, DSFLDI, NTFLDI)
!
!           ^^^^^^  ^^^^^^  ^^^^^^  ^^^^^^  ^^^^^^
         CALL ACCUMI(NLAYER, TRANPI, DUMM, DUMM, DUMM, DUMM, TRANDI, &
               DUMM, DUMM, DUMM, DUMM)
!                                                            ^^^^^^
         CALL ACCUM(SRFLP, SLFLP, GWFLP, SEEPP, DUMMY, SRFLD, SLFLD, &
               GWFLD, SEEPD, DUMMY)
!                                                      ^^^^^  ^^^^^
!           ^^^^^  ^^^^^
