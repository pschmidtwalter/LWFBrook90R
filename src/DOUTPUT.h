!     ******   DOUTPUT.INC   ******
!     daily outputs, flows as amounts in mm
!     Modified: 2.4.04 bsc, comma separated
IF (OP(2, 3) .EQ. 1) THEN
    IF (IDAY .EQ. 1) THEN
        OPEN (UNIT = 31, FILE = 'BUDGDAY.ASC', STATUS='REPLACE')
        WRITE (31,'(''YR,MO,DA,DOY,PREC,MESFL,FLOW,EVAP,SEEP,SNOW,SWAT,GWAT,INTR+S'')')
    END IF
    WRITE (31,'(I4,A1,2(I3,A1),I4,A1,9(F7.1,A1))') YY,',', MONTH,',',DOM,',',DOY,',',PRECD,&
        ',',MESFLD,',',FLOWD,',',EVAPD,',',SEEPD,',',SNOW,',',SWAT,',',GWAT,',',INTR + INTS
END IF


IF (OP(3, 3) .EQ. 1) THEN
    IF (IDAY .EQ. 1) THEN
        OPEN (UNIT = 32, FILE = 'FLOWDAY.ASC', STATUS='REPLACE')
        WRITE (32,'(''YR,MO,DA,DOY,MESFL,FLOW,SEEP,SRFL,SLFL,BYFL,DSFL,GWFL,VRFLN'')')
    END IF
    WRITE (32,'(I4,A1,2(I3,A1),I4,A1,9(F7.1,A1))') YY,',', MONTH,',',DOM,',',DOY,',',MESFLD,&
        ',',FLOWD,',',SEEPD,',',SRFLD,',',SLFLD,',',BYFLD,',',DSFLD,',',GWFLD,',',VRFLDI(NLAYER)
END IF

IF (OP(1, 3) .EQ. 1) THEN
    IF (IDAY .EQ. 1) THEN
        OPEN (UNIT = 39, FILE = 'EVALDAY.ASC', STATUS='REPLACE')
        WRITE (39,'(''YR,MO,DA,DOY,MESFL,FLOW,SNOW,SWAT'')')
    END IF
    WRITE (39,'(I4,A1,2(I3,A1),I4,A1,2(F9.3,A1),2(F8.0,A1))') YY,',',MONTH,',',DOM,',',DOY,&
        ',',MESFLD,',',FLOWD,',',SNOW,',',SWAT
END IF

IF (OP(4, 3) .EQ. 1) THEN
    IF (IDAY .EQ. 1) THEN
        OPEN (UNIT = 33, FILE = 'EVAPDAY.ASC', STATUS='REPLACE')
        WRITE (33,'(''YR,MO,DA,DOY,MESFL,FLOW,EVAP,TRAN,IRVP,ISVP,SLVP,SNVP,PINT,PTRAN,PSLVP'')')
    END IF
    WRITE (33,'(I4,A1,2(I3,A1),I4,A1,11(F7.3,A1))') YY,',',MONTH,',',DOM,',',DOY,',',MESFLD,&
        ',',FLOWD,',',EVAPD,',',TRAND,',',IRVPD,',',ISVPD,',',SLVPD,',',SNVPD,',',PINTD,&
        ',',PTRAND,',',PSLVPD
END IF

IF (OP(5, 3) .EQ. 1) THEN
    IF (IDAY .EQ. 1) THEN
        OPEN (UNIT = 34, FILE = 'ABOVDAY.ASC', STATUS='REPLACE')
        WRITE (34,'(''YR,MO,DA,DOY,RFAL,RINT,SFAL,SINT,RSNO,RNET,SMLT,SLFL,SRFL'')')
    END IF
    WRITE (34,'(I4,A1,2(I3,A1),I4,A1,9(F7.1,A1))') YY,',',MONTH,',',DOM,',',DOY,',',RFALD,&
        ',',RINTD,',',SFALD,',',SINTD,',',RSNOD,',',RNETD,',',SMLTD,',',SLFLD,',',SRFLD
END IF

IF (OP(9, 3) .EQ. 1) THEN
!        source area fraction
    IF (SRFLD + SLFLD .GT. 0.0d0) THEN
        SAFRAC = SRFLD / (SRFLD + SLFLD)
    ELSE
        SAFRAC = 0
    END IF
!        stress factor - ratio of actual to potential transpiration
!         IF (PTRAND .GT. 0) THEN
    IF (PTRAND .GT. 0.0010d0) THEN
        STRES = TRAND / PTRAND
    ELSE
        STRES = 1
    END IF
    IF (STRES .GT. 1.0d0) STRES = 1.0d0
!        root zone soil water and deficit
!         here air content of root zone is calculated and given out instead of ADEF
    AWAT40 = 0.0d0
    AWAT = 0.0d0
    ADEF = 0.0d0
    RootZoneThickness=0.0d0
    DO 15 I = 1, NLAYER
        IF (RELDEN(I) .GE. 1E-06 .AND. RTLEN .GE. .10d0) THEN
!              layer is in root zone
            IF ((FTheta(WETNES(I),Par(1,i),iModel) -  FTheta(WETC(I),Par(1,i),iModel)) .GT. 0) THEN
                AWAT = AWAT + (FTheta(WETNES(I),Par(1,i),iModel) - &
                    FTheta(WETC(I),Par(1,i),iModel)) * SWATMX(I)/Par(1,I)
            END IF
            IF (dep(I) .GE. -0.40d0) AWAT40 = AWAT
                RootZoneThickness=RootZoneThickness+THICK(I)
                ADEF = ADEF + (Par(1,I) - FTheta(WETNES(I),Par(1,i),iModel)) * SWATMX(I)/Par(1,I)
!               ADEF = ADEF + (FTheta(Par(5,i),Par(1,i),iModel) -
!     *          FTheta(WETNES(I),Par(1,i),iModel)) * SWATMX(I)/Par(1,I)
            END IF
15  CONTINUE
    if(RootZoneThickness.gt.0) then
        Eta=ADEF/RootZoneThickness
    else
        Eta=0.0d0
    end if
!        output
    AWATFK = 0.0d0
    IF (IDAY .EQ. 1) THEN
!         AWATFK = 0.0d0
        OPEN (UNIT = 35, FILE = 'MISCDAY.ASC', STATUS='REPLACE')
        WRITE (35,'(''YR,MO,DA,DOY,VRFLN,SAFRAC,STRES,ADEF,AWAT,RELAWAT,AWAT40,NITS,BALERR'')')
        DO 16 I = 1, NLAYER
            dummy =FWETNES(-6.180d0,Par(1,i),iModel)
            IF (RELDEN(I) .GE. 1E-06 .AND. RTLEN .GE. .10d0) THEN
!              layer is in root zone
                IF ((FTheta(dummy,Par(1,i),iModel) -  FTheta(WETC(I),Par(1,i),iModel)) .GT. 0) THEN
                    AWATFK = AWATFK + (FTheta(dummy,Par(1,i),iModel) - &
                        FTheta(WETC(I),Par(1,i),iModel)) * SWATMX(I)/Par(1,I)
                END IF
            END IF
!            Write (*,*) dummy,AWATFK
16     continue
    END IF
!         Pause
    RELAWAT = AWAT / AWATFK
    WRITE (35,'(I4,A1,2(I3,A1),I4,A1,F7.1,A1,2(F7.3,A1),F7.3,A1, F7.1,A1,F5.2,A1,F7.1,A1,I9,A1,F8.4)') &
        YY,',',MONTH,',',DOM,',',DOY,',',VRFLDI(NLAYER),',',SAFRAC,',',STRES,',',Eta,',',&
        AWAT,',',RELAWAT,',',AWAT40,',',NITSD,',',BALERD
END IF

IF (OP(6, 3) .EQ. 1) THEN
    IF (IDAY .EQ. 1) THEN
        OPEN (UNIT = 36, FILE = 'BELODAY.ASC', STATUS='REPLACE')
        WRITE (36,'(''YR,MO,DA,DOY,NL,INFL,BYFL,TRAN,SLVP,VRFL,DSFL,NTFL'')')
    END IF
    WRITE (36,'(I4,A1,2(I3,A1),I4,A1,I3,A1,7(F7.1,A1))') YY,',',MONTH,',',DOM,',',DOY,',',&
        1,',',INFLDI(1),',',BYFLDI(1),',',TRANDI(1),',',SLVPD,',',VRFLDI(1),',',DSFLDI(1),&
        ',',NTFLDI(1)
    DO 23 I = 2, NLAYER
        WRITE (36,'(I4,A1,2(I3,A1),I4,A1,I3,A1,7(F7.1,A1))') YY,',',MONTH,',',DOM,',',DOY,&
           ',',I,',',INFLDI(I),',',BYFLDI(I),',',TRANDI(I),',',0.0d0,',',VRFLDI(I),',',&
           DSFLDI(I),',',NTFLDI(I)
23  CONTINUE
!         WRITE (36,*)
END IF

IF (OP(8, 3) .EQ. 1) THEN
    IF (IDAY .EQ. 1) THEN
        OPEN (UNIT = 37, FILE = 'PSITDAY.ASC', STATUS='REPLACE')
        WRITE (37,'(''YR,MO,DA,DOY,PSIT1,PSIT2,PSIT3,PSIT4,PSIT5,PSIT6,PSIT7'')')
        WRITE (37,'(''       PSIG  '',50F8.1)') (PSIG(I),I=1,NLAYER)
    END IF
    WRITE (37,'(I4,2I3,I4,50F8.1)') YY, MONTH, DOM, DOY, (PSITI(I), I=1,NLAYER)
END IF

IF (OP(7, 3) .EQ. 1) THEN
    IF (IDAY .EQ. 1) THEN
        OPEN (UNIT = 38, FILE = 'SWATDAY.ASC', STATUS='REPLACE')
        WRITE (38,'(''YR,MO,DA,DOY,NL,SWATI,THETA,WETNES,PSIMI,PSITI,TEMPERATURE'')')
    END IF
    DO 33 I = 1, NLAYER
        A0=Max(-9999.00d0,Psim(i))
        A1=Max(-9999.00d0,Psiti(i))
        IF (HEAT .EQ. 1) THEN
            WRITE (38,'(I4,A1,2(I3,A1),I4,A1,I5,A1,F7.1,A1,2(F7.3,A1),2(F8.1,A1),F7.2,A1)')YY,',',&
                MONTH,',', DOM,',', DOY,',', I,',', SWATI(I),',', THETA(I),&
                ',',WETNES(I),',', A0,',', A1,',',TMean(i)
            TMean(i) = 0
        ELSE
            WRITE (38,'(I4,A1,2(I3,A1),I4,A1,I5,A1,F7.1,A1,2(F7.3,A1),2(F8.1,A1))') YY,',', MONTH,',', &
                DOM,',', DOY,',', I,',', SWATI(I),',', THETA(I),',', WETNES(I),',',A0,',', A1
        END IF
33  CONTINUE
!         WRITE (38,*)
END IF


