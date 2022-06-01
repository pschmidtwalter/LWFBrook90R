! 2020.04.20 Daily output directly to R.
! developed by V. Trotsiuk volodymyr.trostiuk@wsl.ch


output_day( IDAY, 1) = YY
output_day( IDAY, 2) = MM
output_day( IDAY, 3) = DD
output_day( IDAY, 4) = DOY

output_day( IDAY, 5) = RFALD
output_day( IDAY, 6) = RINTD
output_day( IDAY, 7) = SFALD
output_day( IDAY, 8) = SINTD
output_day( IDAY, 9) = RTHRD
output_day( IDAY, 10) = STHRD
output_day( IDAY, 11) = RSNOD
output_day( IDAY, 12) = RNETD
output_day( IDAY, 13) = SMLTD

output_day( IDAY, 14) = SNOW
output_day( IDAY, 15) = SWAT
output_day( IDAY, 16) = GWAT
output_day( IDAY, 17) = INTR
output_day( IDAY, 18) = INTS
output_day( IDAY, 19) = EVAPD
output_day( IDAY, 20) = TRAND
output_day( IDAY, 21) = IRVPD
output_day( IDAY, 22) = ISVPD
output_day( IDAY, 23) = SLVPD
output_day( IDAY, 24) = SNVPD
output_day( IDAY, 25) = PINTD
output_day( IDAY, 26) = PTRAND
output_day( IDAY, 27) = PSLVPD
output_day( IDAY, 28) = FLOWD
output_day( IDAY, 29) = SEEPD
output_day( IDAY, 30) = SRFLD
output_day( IDAY, 31) = SLFLD
output_day( IDAY, 32) = BYFLD
output_day( IDAY, 33) = DSFLD
output_day( IDAY, 34) = GWFLD
output_day( IDAY, 35) = VRFLDI(NLAYER)


IF (SRFLD + SLFLD .GT. 0.d0) THEN
    SAFRAC = SRFLD / (SRFLD + SLFLD)
ELSE
    SAFRAC = 0
END IF

!stress factor - ratio of actual to potential transpiration
IF (PTRAND .GT. 0.001d0) THEN
    STRES = TRAND / PTRAND
ELSE
    STRES = 1
END IF
IF (STRES .GT. 1.d0) STRES = 1.d0

! root zone soil water and deficit
! here air content of root zone is calculated and given out instead of ADEF
AWAT40 = 0.d0
AWAT = 0.d0
ADEF = 0.d0
RootZoneThickness=0.d0
DO 915 I = 1, NLAYER
    IF (RELDEN(I) .GE. 1E-06 .AND. RTLEN .GE. .1d0) THEN
    ! layer is in root zone
        IF ((FTheta(WETNES(I),Par(1,i),iModel) -  FTheta(WETC(I),Par(1,i),iModel)) .GT. 0) THEN
            AWAT = AWAT + (FTheta(WETNES(I),Par(1,i),iModel) - &
                FTheta(WETC(I),Par(1,i),iModel)) * SWATMX(I)/Par(1,I)
        END IF

        IF (dep(I) .GE. -0.4d0) AWAT40 = AWAT
            RootZoneThickness=RootZoneThickness+THICK(I)
            ADEF = ADEF + (Par(1,I) - FTheta(WETNES(I),Par(1,i),iModel)) * SWATMX(I)/Par(1,I)
        END IF
915  CONTINUE

if(RootZoneThickness.gt.0) then
    Eta=ADEF/RootZoneThickness
else
    Eta=0.d0
end if


IF (IDAY .EQ. 1) THEN
    AWATFK = 0.0d0
    DO 916 I = 1, NLAYER
        dummy = FWETNES(-6.18d0,Par(1,i),iModel)
        IF (RELDEN(I) .GE. 1E-06 .AND. RTLEN .GE. .1d0) THEN
            ! layer is in root zone
            IF ((FTheta(dummy,Par(1,i),iModel) -  FTheta(WETC(I),Par(1,i),iModel)) .GT. 0) THEN
                AWATFK = AWATFK + (FTheta(dummy,Par(1,i),iModel) - &
                    FTheta(WETC(I),Par(1,i),iModel)) * SWATMX(I)/Par(1,I)
            END IF
        END IF
916  continue
END IF

RELAWAT = AWAT / AWATFK

output_day( IDAY, 36) = SAFRAC
output_day( IDAY, 37) = STRES
output_day( IDAY, 38) = Eta
output_day( IDAY, 39) = AWAT
output_day( IDAY, 40) = RELAWAT
output_day( IDAY, 41) = NITSD
output_day( IDAY, 42) = BALERD
output_day( IDAY, 43) = SLFDAY * SOLRAD
output_day( IDAY, 44) = SLFDAY * SOLRAD * (1-ALBEDO)
output_day( IDAY, 45) = (LNGNET(1) * DAYLEN + LNGNET(2) * (1.0d0 - DAYLEN)) / DT
output_day( IDAY, 46) = (AA(1) * DAYLEN + AA(2) * (1.0d0 - DAYLEN)) / DT
output_day( IDAY, 47) = (ASUBS(1) * DAYLEN + ASUBS(2) * (1.0d0 - DAYLEN)) / DT
