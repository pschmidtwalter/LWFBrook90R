! 2020.04.20 Daily output directly to R.
! developed by V. Trotsiuk volodymyr.trostiuk@wsl.ch


output_day( (1+NPINT*(IDAY-1)+(N-1)), 1) = YY
output_day( (1+NPINT*(IDAY-1)+(N-1)), 2) = MM
output_day( (1+NPINT*(IDAY-1)+(N-1)), 3) = DD
output_day( (1+NPINT*(IDAY-1)+(N-1)), 4) = DOY

output_day( (1+NPINT*(IDAY-1)+(N-1)), 5) = RFAL
output_day( (1+NPINT*(IDAY-1)+(N-1)), 6) = RINT
output_day( (1+NPINT*(IDAY-1)+(N-1)), 7) = SFAL
output_day( (1+NPINT*(IDAY-1)+(N-1)), 8) = SINT
output_day( (1+NPINT*(IDAY-1)+(N-1)), 9) = RTHR
output_day( (1+NPINT*(IDAY-1)+(N-1)), 10) = STHR
output_day( (1+NPINT*(IDAY-1)+(N-1)), 11) = RSNO
output_day( (1+NPINT*(IDAY-1)+(N-1)), 12) = RNET
output_day( (1+NPINT*(IDAY-1)+(N-1)), 13) = SMLT

output_day( (1+NPINT*(IDAY-1)+(N-1)), 14) = SNOW
output_day( (1+NPINT*(IDAY-1)+(N-1)), 15) = SWAT
output_day( (1+NPINT*(IDAY-1)+(N-1)), 16) = GWAT
output_day( (1+NPINT*(IDAY-1)+(N-1)), 17) = INTR
output_day( (1+NPINT*(IDAY-1)+(N-1)), 18) = INTS
output_day( (1+NPINT*(IDAY-1)+(N-1)), 19) = EVAPP / DTP
output_day( (1+NPINT*(IDAY-1)+(N-1)), 20) = TRANP /DTP
output_day( (1+NPINT*(IDAY-1)+(N-1)), 21) = IRVP 
output_day( (1+NPINT*(IDAY-1)+(N-1)), 22) = ISVP
output_day( (1+NPINT*(IDAY-1)+(N-1)), 23) = SLVP
output_day( (1+NPINT*(IDAY-1)+(N-1)), 24) = SNVP
output_day( (1+NPINT*(IDAY-1)+(N-1)), 25) = PINT
output_day( (1+NPINT*(IDAY-1)+(N-1)), 26) = PTRAN
output_day( (1+NPINT*(IDAY-1)+(N-1)), 27) = PSLVP / DTP
output_day( (1+NPINT*(IDAY-1)+(N-1)), 28) = FLOWP / DTP
output_day( (1+NPINT*(IDAY-1)+(N-1)), 29) = SEEPP / DTP
output_day( (1+NPINT*(IDAY-1)+(N-1)), 30) = SRFLP / DTP
output_day( (1+NPINT*(IDAY-1)+(N-1)), 31) = SLFLP / DTP
output_day( (1+NPINT*(IDAY-1)+(N-1)), 32) = BYFLP / DTP
output_day( (1+NPINT*(IDAY-1)+(N-1)), 33) = DSFLP / DTP
output_day( (1+NPINT*(IDAY-1)+(N-1)), 34) = GWFLP / DTP
output_day( (1+NPINT*(IDAY-1)+(N-1)), 35) = VRFLPI(NLAYER) / DTP


IF (SRFLP + SLFLP .GT. 0.d0) THEN
    SAFRAC = SRFLP / (SRFLP + SLFLP)
ELSE
    SAFRAC = 0
END IF

!stress factor - ratio of actual to potential transpiration
IF (PTRAN .GT. 0.001d0) THEN
    STRES = TRANP / PTRAN ! hier vielleicht TRANP/DTP / NPINT?
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

output_day( (1+NPINT*(IDAY-1)+(N-1)), 36) = SAFRAC
output_day( (1+NPINT*(IDAY-1)+(N-1)), 37) = STRES
output_day( (1+NPINT*(IDAY-1)+(N-1)), 38) = Eta
output_day( (1+NPINT*(IDAY-1)+(N-1)), 39) = AWAT
output_day( (1+NPINT*(IDAY-1)+(N-1)), 40) = RELAWAT
output_day( (1+NPINT*(IDAY-1)+(N-1)), 41) = NITS
output_day( (1+NPINT*(IDAY-1)+(N-1)), 42) = BALERD
output_day( (1+NPINT*(IDAY-1)+(N-1)), 43) = SLFDAY * SOLRAD
output_day( (1+NPINT*(IDAY-1)+(N-1)), 44) = SLFDAY * SOLRAD * (1-ALBEDO)
output_day( (1+NPINT*(IDAY-1)+(N-1)), 45) = (LNGNET(1) * DAYLEN + LNGNET(2) * (1.0d0 - DAYLEN)) / DT
output_day( (1+NPINT*(IDAY-1)+(N-1)), 46) = (AA(1) * DAYLEN + AA(2) * (1.0d0 - DAYLEN)) / DT
output_day( (1+NPINT*(IDAY-1)+(N-1)), 47) = (ASUBS(1) * DAYLEN + ASUBS(2) * (1.0d0 - DAYLEN)) / DT
