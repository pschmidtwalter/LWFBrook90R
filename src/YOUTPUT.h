!     ******   YOUTPUT.INC   ******
!     annual outputs, flows as amounts in mm

IF (OP(2,1) .EQ. 1) THEN
    IF (IDAY .LT. 367) THEN
        OPEN (UNIT = 51, FILE = 'BUDGANN.ASC', STATUS='REPLACE')
        WRITE (51,'(''YR,PREC, MESFL,  FLOW,  EVAP,SEEP,SNOW,SWAT,GWAT,INTR+S'')')
    END IF
    WRITE (51,'(I4,A1,9(F7.1,A1))') YY,',',PRECY,',',MESFLY,',',FLOWY,',',EVAPY,',', &
        SEEPY,',', SNOW,',', SWAT,',', GWAT,',',INTR + INTS
END IF

IF (OP(3,1) .EQ. 1) THEN
    IF (IDAY .LT. 367) THEN
        OPEN (UNIT = 52, FILE = 'FLOWANN.ASC', STATUS='REPLACE')
        WRITE (52,'(''YR,MESFL,FLOW,SEEP,SRFL,SLFL,BYFL,DSFL,GWFL,VRFLN'')')
    END IF
    WRITE (52,'(I4,A1,9(F7.1,A1))') YY,',', MESFLY,',', FLOWY,',',SEEPY,',',SRFLY,',', &
        SLFLY,',', BYFLY,',', DSFLY,',',GWFLY,',', VRFLYI(NLAYER)
END IF

IF (OP(1,1) .EQ. 1) THEN
    IF (IDAY .LT. 367) THEN
        OPEN (UNIT = 59, FILE = 'EVALANN.ASC', STATUS='REPLACE')
        WRITE (59,'(''YR,MESFL,FLOW,SNOW,SWAT'')')
    END IF
    WRITE (59,'(I4,A1,2(F9.3,A1),2(F8.0,A1))') YY,',', MESFLY,',', FLOWY,',', SNOW,',',SWAT
END IF

IF (OP(4,1) .EQ. 1) THEN
    IF (IDAY .LT. 367) THEN
        OPEN (UNIT = 53, FILE = 'EVAPANN.ASC', STATUS='REPLACE')
        WRITE (53,'(''YR,MESFL,FLOW,EVAP,TRAN,IRVP,ISVP,SLVP,SNVP,PINT,PTRAN,PSLVP'')')
    END IF
        WRITE (53,'(I4,A1,11(F7.1,A1))') YY,',', MESFLY,',',FLOWY,',', EVAPY,',',TRANY,',', &
            IRVPY,',', ISVPY,',', SLVPY,',',SNVPY,',', PINTY,',', PTRANY,',',PSLVPY
END IF

IF (OP(4,1) .EQ. 1) THEN
    IF (IDAY .LT. 367) THEN
        OPEN (UNIT = 60, FILE = 'EPVPANN.ASC', STATUS='REPLACE')
        WRITE (60,'(''YR,MESFL,FLOW,EVAP,TRAN,IRVP,ISVP,SLVP,SNVP,PINT,PTRAN,PSLVP,ETDif'')')
    END IF
    ETDiff=(PTRANVP+PSLVPVP)-(TRANVP+SLVPVP)
    WRITE (60,'(I4,A1,12(F7.1,A1))') YY,',', MESFLVP,',',FLOWVP,',', EVAPVP,',',TRANVP,',', &
        IRVPVP,',', ISVPVP,',',SLVPVP,',',SNVPVP,',', PINTVP,',', PTRANVP,',',PSLVPVP ,',', &
        ETDiff
END IF

IF (OP(5,1) .EQ. 1) THEN
    IF (IDAY .LT. 367) THEN
        OPEN (UNIT = 54, FILE = 'ABOVANN.ASC', STATUS='REPLACE')
        WRITE (54,'(''YR,RFAL,RINT,SFAL,SINT,RSNO,RNET,SMLT,SLFL,SRFL'')')
    END IF
        WRITE (54,'(I4,A1,9(F7.1,A1))') YY,',', RFALY,',', RINTY,',',SFALY,',', SINTY,',',&
            RSNOY,',', RNETY,',', SMLTY,',', SLFLY,',', SRFLY
END IF

IF (OP(9,1) .EQ. 1) THEN
!        water balance error
    BALERY = STORY - (INTR + INTS + SNOW + SWAT + GWAT) + PRECY - EVAPY - FLOWY - SEEPY
    STORY = INTR + INTS + SNOW + SWAT + GWAT
!        source area fraction
    IF (SRFLY + SLFLY .GT. 0) THEN
        SAFRAC = SRFLY / (SRFLY + SLFLY)
    ELSE
        SAFRAC = 0
    END IF
!        stress factor - ratio of actual to potential transpiration
    IF (PTRANY .GT. 0) THEN
        STRES = TRANY / PTRANY
    ELSE
        STRES = 1
    END IF
    IF (PTRANVP .GT. 0) THEN
        STRESVP = TRANVP / PTRANVP
    ELSE
        STRES = 1
    END IF
    IF (IDAY .LT. 367) THEN
        OPEN (UNIT = 55, FILE = 'MISCANN.ASC', STATUS='REPLACE')
        WRITE (55,'(''YR,VRFLN,SAFRAC,STRES,STRESVP,NoPar1,NoPar2,NITS, BALERR'')')
    END IF
    WRITE (55,'(I4,A1,F7.1,A1,3(F7.3,A1),2(F7.1,A1),I9,A1,F8.4)') YY,',',VRFLYI(NLAYER),',', &
        SAFRAC,',', STRES,',',STRESVP,',', 0.,',',0.,',',NITSY,',', BALERY
END IF

IF (OP(6,1) .EQ. 1) THEN
    IF (IDAY .LT. 367) THEN
        OPEN (UNIT = 56, FILE = 'BELOANN.ASC', STATUS='REPLACE')
        WRITE (56,'(''YR,NL,INFL,BYFL,TRAN,SLVP,VRFL,DSFL,NTFL'')')
    END IF
    WRITE (56,'(I4,A1,I3,A1,7(F7.1,A1))') YY,',', 1,',',INFLYI(1),',', BYFLYI(1),',', &
        TRANYI(1),',', SLVPY,',', VRFLYI(1),',', DSFLYI(1),',', NTFLYI(1)
    DO 17 I = 2, NLAYER
        WRITE (56,'(I4,A1,I3,A1,7(F7.1,A1))') YY,',', I,',',INFLYI(I),',',BYFLYI(I),',', &
             TRANYI(I),',', 0.,',',VRFLYI(I),',', DSFLYI(I),',', NTFLYI(I)
17  CONTINUE
        WRITE (56,*)
END IF

IF (OP(8,1) .EQ. 1) THEN
    IF (IDAY .LT. 367) THEN
        OPEN (UNIT = 57, FILE = 'PSITANN.ASC', STATUS='REPLACE')
        WRITE (57,'(''YR PSIT1 PSIT2 PSIT3 PSIT4 PSIT5 PSIT6 PSIT7...'')')
        WRITE (57,'(''       PSIG  '',50F8.1)')(PSIG(I),I=1,NLAYER)
    END IF
    WRITE (57,'(I4,10X,50F8.1)') YY, (PSITI(I), I=1,NLAYER)
END IF

IF (OP(7,1) .EQ. 1) THEN
    IF (IDAY .LT. 367) THEN
        OPEN (UNIT = 58, FILE = 'SWATANN.ASC', STATUS='REPLACE')
        WRITE (58,'(''YR,NL,SWATI,THETA,WETNES,PSIMI,PSITI,TEMPERATURE'')')
    END IF
    DO 25 I = 1, NLAYER
        A0=Max(-9999.00d0,Psim(i))
        A1=Max(-9999.00d0,Psiti(i))
        IF (HEAT .EQ. 1) THEN
            WRITE (58,'(I4,A1,10X,I3,A1,F7.1,A1, 2(F7.3,A1),2(F8.1,A1),F7.2)') YY,',', I,',',&
            SWATI(I),',', THETA(I),',', WETNES(I),',',B0,',',B1,',',TemperatureNew(NLAYER+1-i)
        ELSE
        WRITE (58,'(I4,A1,10X,I3,A1,F7.1,A1, 2(F7.3,A1),2(F8.1,A1))') YY,',', I,',',&
            SWATI(I),',', THETA(I),',', WETNES(I),',',B0,',',B1
        END IF
25  CONTINUE
    WRITE (58,*)
END IF
