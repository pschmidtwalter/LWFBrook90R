!     ******   YACCUM.INC   ******
!        accumulate flows over year
!        zeroed by ZYEAR.INC
         CALL ACCUMI(NLAYER, VRFLMI, INFLMI, BYFLMI, DSFLMI, NTFLMI, &
           VRFLYI, INFLYI, BYFLYI, DSFLYI, NTFLYI)

!           ^^^^^^  ^^^^^^  ^^^^^^  ^^^^^^  ^^^^^^
         CALL ACCUMI(NLAYER, TRANMI, DUMM, DUMM, DUMM, DUMM, TRANYI, &
           DUMM, DUMM, DUMM, DUMM)
!        
         IF (MONTH .GE. 5 .AND. MONTH .LE. 8) THEN
         CALL ACCUMI(NLAYER, TRANMI, DUMM, DUMM, DUMM, DUMM, TRANVPI, &
           DUMM, DUMM, DUMM, DUMM)
         END IF 
!                                                            ^^^^^^
         CALL ACCUM(SRFLM, SLFLM, GWFLM, SEEPM, DUMMY, SRFLY, SLFLY, &
           GWFLY, SEEPY, DUMMY)
!                                                      ^^^^^  ^^^^^
!           ^^^^^  ^^^^^
         CALL ACCUM(ISVPM, IRVPM, SNVPM, SLVPM, SFALM, ISVPY, IRVPY, &
           SNVPY, SLVPY, SFALY)
!                                                      ^^^^^  ^^^^^
!           ^^^^^  ^^^^^  ^^^^^
         IF (MONTH .GE. 5 .AND. MONTH .LE. 8) THEN
         CALL ACCUM(ISVPM, IRVPM, SNVPM, SLVPM, DUMMY, ISVPVP, IRVPVP, &
           SNVPVP, SLVPVP, DUMMY)
         END IF
!                                                      ^^^^^  ^^^^^
!           ^^^^^  ^^^^^  ^^^^^
         CALL ACCUM(RFALM, SINTM, RINTM, RSNOM, SMLTM, RFALY, SINTY, &
           RINTY, RSNOY, SMLTY)
!                                                      ^^^^^  ^^^^^
!           ^^^^^  ^^^^^  ^^^^^
         CALL ACCUM(MESFLM, PTRANM, PINTM, PSLVPM, DUMMY, MESFLY, &
           PTRANY, PINTY,PSLVPY, DUMMY)
!                                                        ^^^^^^
!           ^^^^^^  ^^^^^
         IF (MONTH .GE. 5 .AND. MONTH .LE. 8) THEN
         CALL ACCUM(MESFLM, PTRANM, PINTM, PSLVPM, DUMMY, MESFLVP, &
           PTRANVP, PINTVP,PSLVPVP, DUMMY)
         END IF
!                                                        ^^^^^^
!           ^^^^^^  ^^^^^
