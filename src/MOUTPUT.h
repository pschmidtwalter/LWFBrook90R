!     ******   MOUTPUT.INC   ******
!     monthly outputs, flows as amounts in mm

IF (OP(2,2) .EQ. 1) THEN
    IF (IDAY .LT. 32) THEN
        OPEN (UNIT = 41, FILE = 'out/BUDGMON.csv', STATUS='REPLACE')
        WRITE(41,'(''YR,MO,PREC,MESFL,FLOW,EVAP,SEEP,SNOW,SWAT,GWAT,INTR+S'')')
    END IF
    WRITE (41,'(I4,A1,I3,A1,9(F7.1,A1))') YY,',', MONTH,',',PRECM,',', MESFLM,',',FLOWM,',',&
        EVAPM,',', SEEPM,',', SNOW, ',',SWAT,',', GWAT,',', INTR + INTS
END IF

IF (OP(3,2) .EQ. 1) THEN
    IF (IDAY .LT. 32) THEN
        OPEN (UNIT = 42, FILE = 'out/FLOWMON.csv', STATUS='REPLACE')
        WRITE (42,'(''YR,MO,MESFL,FLOW,SEEP,SRFL,SLFL,BYFL,DSFL,GWFL,VRFLN'')')
    END IF
    WRITE (42,'(I4,A1,I3,A1,9(F7.1,A1))') YY,',', MONTH,',', MESFLM,',', FLOWM,',',SEEPM,',',&
        SRFLM,',', SLFLM,',', BYFLM, ',', DSFLM,',', GWFLM,',', VRFLMI(NLAYER)
END IF

IF (OP(1,2) .EQ. 1) THEN
    IF (IDAY .LT. 32) THEN
        OPEN (UNIT = 49, FILE = 'out/EVALMON.csv', STATUS='REPLACE')
        WRITE (49,'(''YR,MO,MESFL,FLOW,SNOW,SWAT'')')
    END IF
    WRITE (49,'(I4,A1,I3,A1,2(F9.3,A1),2(F8.0,A1))') YY,',',MONTH,',', MESFLM,',',&
        FLOWM,',', SNOW,',', SWAT
END IF

IF (OP(4,2) .EQ. 1) THEN
    IF (IDAY .LT. 32) THEN
        OPEN (UNIT = 43, FILE = 'out/EVAPMON.csv', STATUS='REPLACE')
        WRITE (43,'(''YR,MO,MESFL,FLOW,EVAP,TRAN,IRVP,ISVP,SLVP,SNVP,PINT,PTRAN,PSLVP'')')
    END IF
    WRITE (43,'(I4,A1,I3,A1,11(F7.1,A1))') YY,',', MONTH,',',MESFLM,',', FLOWM,',',&
        EVAPM,',', TRANM,',', IRVPM,',', ISVPM,',',SLVPM,',', SNVPM,',', PINTM,',', PTRANM,',',PSLVPM
END IF

IF (OP(5,2) .EQ. 1) THEN
    IF (IDAY .LT. 32) THEN
        OPEN (UNIT = 44, FILE = 'out/ABOVMON.csv', STATUS='REPLACE')
        WRITE (44,'(''YR,MO,RFAL,RINT,SFAL,SINT,RSNO,RNET,SMLT,SLFL,SRFL'')')
    END IF
    WRITE (44,'(I4,A1,I3,A1,9(F7.1,A1))') YY,',', MONTH,',',RFALM,',', RINTM,',',SFALM,',',&
         SINTM,',', RSNOM,',', RNETM,',',SMLTM,',', SLFLM,',', SRFLM
END IF

IF (OP(9,2) .EQ. 1) THEN
!        water balance error
    BALERM = STORM - (INTR + INTS + SNOW + SWAT + GWAT) + PRECM - EVAPM - FLOWM - SEEPM
    STORM = INTR + INTS + SNOW + SWAT + GWAT
!        source area fraction
    IF (SRFLM + SLFLM .GT. 0) THEN
        SAFRAC = SRFLM / (SRFLM + SLFLM)
    ELSE
            SAFRAC = 0.0d0
    END IF
!        stress factor - ratio of actual to potential transpiration
    IF (PTRANM .GT. 0.0d0) THEN
        STRES = TRANM / PTRANM
    ELSE
        STRES = 1.0d0
    END IF
!        root zone soil water and deficit
!        output
    IF (IDAY .LT. 32) THEN
        OPEN (UNIT = 45, FILE = 'out/MISCMON.csv', STATUS='REPLACE')
        WRITE (45,'(''YR,MO,VRFLN,SAFRAC,STRES,NITS,BALERR'')')
    END IF
    WRITE (45,'(I4,A1,I3,A1,F7.1,A1,2(F7.3,A1),I9,A1,F8.4)') YY,',',MONTH,',',&
         VRFLMI(NLAYER),',', SAFRAC,',', STRES,',', NITSM,',',BALERM
END IF

IF (OP(6,2) .EQ. 1) THEN
    IF (IDAY .LT. 32) THEN
        OPEN (UNIT = 46, FILE = 'out/BELOMON.csv', STATUS='REPLACE')
        WRITE (46,'(''YR,MO,NL,INFL,BYFL,TRAN,SLVP,VRFL,DSFL,NTFL'')')
    END IF
    WRITE (46,'(I4,A1,I3,A1,I3,A1,7(F7.1,A1))') YY,',', MONTH,',', 1, ',', INFLMI(1),','&
        ,BYFLMI(1),',', TRANMI(1),',', SLVPM,',',VRFLMI(1),',', DSFLMI(1),',', NTFLMI(1)
    DO 24 I = 2, NLAYER
        WRITE (46,'(I4,A1,I3,A1,I3,A1,7(F7.1,A1))') YY,',',MONTH,',', I,',',INFLMI(I),',',&
            BYFLMI(I),',', TRANMI(I),',', 0.,',', VRFLMI(I),',',DSFLMI(I),',', NTFLMI(I)
24  CONTINUE
!      WRITE (46,*)
END IF

IF (OP(8,2) .EQ. 1) THEN
    IF (IDAY .LT. 32) THEN
        OPEN (UNIT = 47, FILE = 'out/PSITMON.csv', STATUS='REPLACE')
        WRITE (47,'(''YR   MO PSIT1   PSIT2   PSIT3   PSIT4   PSIT5   PSIT6   PSIT7...'')')
        WRITE (47,'(''       PSIG  '',50F8.1)')(PSIG(I),I=1,NLAYER)
    END IF
    WRITE (47,'(I4,I3,50F8.1)') YY, MONTH,(PSITI(I), I=1,NLAYER)
END IF

IF (OP(7,2) .EQ. 1) THEN
    IF (IDAY .LT. 32) THEN
        OPEN (UNIT = 48, FILE = 'out/SWATMON.csv', STATUS='REPLACE')
        WRITE (48,'(''YR,MO,NL,SWATI,THETA,WETNES,PSIMI,PSITI,TEMPERATURE'')')
    END IF
    DO 34 I = 1, NLAYER
        A0=Max(-9999.00d0,Psim(i))
        A1=Max(-9999.00d0,Psiti(i))
        IF (HEAT .EQ. 1) THEN
            WRITE (48,'(I4,A1,I3,A1,I3,A1,F7.1,A1,2(F7.3,A1),2(F8.1,A1),F7.2)') YY,',',&
                MONTH,',', I,',', SWATI(I),',', THETA(I),',', WETNES(I),',', A0,',', A1,',',&
                TemperatureNew(NLAYER+1-i)
        ELSE
            WRITE (48,'(I4,A1,I3,A1,I3,A1,F7.1,A1,2(F7.3,A1),2(F8.1,A1))') YY,',',MONTH,',',&
                 I,',', SWATI(I),',', THETA(I),',', WETNES(I),',', A0,',', A1
        END IF
!            WRITE (48,'(I4,I3,7X,I3,F7.1,2F7.3,2F8.1)') YY,
!     *         MONTH, I, SWATI(I), THETA(I), WETNES(I), A0, A1
34  CONTINUE
!         WRITE (48,*)
END IF
