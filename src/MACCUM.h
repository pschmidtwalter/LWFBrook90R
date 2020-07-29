!     ******   MACCUM.INC   ******
!     accumulate flows over month
!     zeroed by ZMONTH.INC
      CALL ACCUMI(NLAYER, VRFLDI, INFLDI, BYFLDI, DSFLDI, NTFLDI,& 
        VRFLMI, INFLMI, BYFLMI, DSFLMI, NTFLMI)

!        ^^^^^^  ^^^^^^  ^^^^^^  ^^^^^^  ^^^^^^
      CALL ACCUMI(NLAYER, TRANDI, DUMM, DUMM, DUMM, DUMM, TRANMI, DUMM, &
        DUMM, DUMM, DUMM)
!                                                         ^^^^^^
      CALL ACCUM(SRFLD, SLFLD, GWFLD, SEEPD, DUMMY, SRFLM, SLFLM, &
        GWFLM, SEEPM, DUMMY)
!                                                   ^^^^^  ^^^^^
!        ^^^^^  ^^^^^
      CALL ACCUM(ISVPD, IRVPD, SNVPD, SLVPD, SFALD, ISVPM, IRVPM, &
        SNVPM, SLVPM, SFALM)
!                                                   ^^^^^  ^^^^^
!        ^^^^^  ^^^^^  ^^^^^
      CALL ACCUM(RFALD, SINTD, RINTD, RSNOD, SMLTD, RFALM, SINTM, &
        RINTM, RSNOM, SMLTM)
!                                                   ^^^^^  ^^^^^
!        ^^^^^  ^^^^^  ^^^^^
      CALL ACCUM(MESFLD, PTRAND, PINTD, PSLVPD, DUMMY, MESFLM, PTRANM, &
        PINTM, PSLVPM, DUMMY)
!                                                     ^^^^^^  ^^^^^^
!        ^^^^^
