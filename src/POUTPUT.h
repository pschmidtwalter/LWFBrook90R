!     ******   POUTPUT.INC   ******
!     precipitation interval output, flows as rates in mm/d

IF (OP(3, 4) .EQ. 1) THEN
    IF (IDAY .EQ. 1 .AND. N .EQ. 1) THEN
        OPEN (UNIT = 22, FILE = 'FLOW.PRE', STATUS='REPLACE')
        WRITE (22,'(''YR,MO,DA,DOY,NP,MESFL,FLOW,SEEP,SRFL,SLFL,BYFL,DSFL,GWFL,VRFLN'')')
    END IF
    WRITE (22,'(I4,A1,2(I3,A1),2(I4,A1),9(F7.2,A1))') YEAR,',', MONTH,',', DOM,',', DOY,',', &
        N,',', MESFLP / DTP,',', FLOWP / DTP,',', SEEPP / DTP,',', SRFLP / DTP,',', &
        SLFLP / DTP,',', BYFLP / DTP,',', DSFLP / DTP,',', GWFLP / DTP,',', VRFLPI(NLAYER) / DTP
END IF

IF (OP(4, 4) .EQ. 1) THEN
    IF (IDAY .EQ. 1 .AND. N .EQ. 1) THEN
        OPEN (UNIT = 23, FILE = 'EVAP.PRE', STATUS='REPLACE')
        WRITE (23,'(''YR,MO,DA,DOY,NP,MESFL,FLOW,EVAP,TRAN,IRVP,ISVP,SLVP,SNVP,PINT,PTRAN'')')
    END IF
    WRITE (23,'(I4,A1,2(I3,A1),2(I4,A1),10(F7.2,A1))') YEAR,',',MONTH,',',DOM,',',DOY,',', &
        N,',',MESFLP / DTP,',',FLOWP / DTP,',',EVAPP / DTP,',',TRANP / DTP,',',IRVP,',', &
        ISVP,',',SLVP,',',SNVP,',',PINT,',',PTRAN
END IF

IF (OP(5, 4) .EQ. 1) THEN
    IF (IDAY .EQ. 1 .AND. N .EQ. 1) THEN
        OPEN (UNIT = 24, FILE = 'ABOV.PRE', STATUS='REPLACE')
        WRITE (24,'(''YR,MO,DA,DOY,NP,RFAL,RINT,SFAL,SINT,RSNO,RNET,SMLT,SLFL,SRFL'')')
    END IF
    WRITE (24,'(I4,A1,2(I3,A1),2(I4,A1),9(F7.2,A1))') YEAR,',',MONTH,',',DOM,',',DOY,',',N, ',',&
        RFAL,',',RINT,',',SFAL,',',SINT,',',RSNO,',',RNET,',',SMLT,',',SLFLP / DTP,',',SRFLP / DTP
END IF

IF (OP(6, 4) .EQ. 1) THEN
    IF (IDAY .EQ. 1 .AND. N .EQ. 1) THEN
        OPEN (UNIT = 26, FILE = 'BELO.PRE', STATUS='REPLACE')
        WRITE (26,'(''YR,MO,DA,DOY,NP,NL,INFL,BYFL,TRAN,SLVP,VRFL,DSFL,NTFL'')')
    END IF
    WRITE (26,'(I4,A1,2(I3,A1),2(I4,A1),I3,A1,7(F7.2,A1))') YEAR,',',MONTH,',',DOM,',',DOY,',',&
        N,',',1,',',INFLPI(1) / DTP,',',BYFLPI(1) / DTP,',',TRANPI(1) / DTP,',',SLVP,',', &
        VRFLPI(1) / DTP,',',DSFLPI(1) / DTP,',',NTFLPI(1) / DTP
    DO 13 I = 2, NLAYER
        WRITE (26,'(I4,A1,2(I3,A1),2(I4,A1),I3,A1,7(F7.2,A1))') YEAR,',',MONTH,',',DOM,',',DOY,',', &
            N,',',I,',',INFLPI(I) / DTP,',',BYFLPI(I) / DTP,',',TRANPI(I) / DTP,',',0.0d0,',',&
            VRFLPI(I) / DTP,',',DSFLPI(I) / DTP,',',NTFLPI(I) / DTP
13  CONTINUE
    WRITE (26,*)
END IF

IF (OP(8, 4) .EQ. 1) THEN
    IF (IDAY .EQ. 1 .AND. N .EQ. 1) THEN
        OPEN (UNIT = 27, FILE = 'PSIT.PRE', STATUS='REPLACE')
        WRITE (27,'(''YR   MO DA DOY  NP   PSIT1   PSIT2   PSIT3   PSIT4   PSIT5   PSIT6   PSIT7...'')')
        WRITE (27,'(''    PSIG         '', 50F8.1)') (PSIG(I), I=1,NLAYER)
    END IF
    WRITE (27,'(I4,2I3,2I4,50F8.1)') YEAR, MONTH, DOM, DOY, N,(PSITI(I), I=1,NLAYER)
END IF
