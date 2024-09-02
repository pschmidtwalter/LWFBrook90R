
output_layer( (1+NPINT*(IDAY-1)+(N-1)), 1, : ) = YY
output_layer( (1+NPINT*(IDAY-1)+(N-1)), 2, : ) = MM
output_layer( (1+NPINT*(IDAY-1)+(N-1)), 3, : ) = DD
output_layer( (1+NPINT*(IDAY-1)+(N-1)), 4, : ) = DOY

output_layer( (1+NPINT*(IDAY-1)+(N-1)), 5, : ) = SWATI(1:NLAYER)
output_layer( (1+NPINT*(IDAY-1)+(N-1)), 6, : ) = THETA(1:NLAYER)
output_layer( (1+NPINT*(IDAY-1)+(N-1)), 7, : ) = WETNES(1:NLAYER)
output_layer( (1+NPINT*(IDAY-1)+(N-1)), 8, : ) = max(-9999.d0, PSIM(1:NLAYER))

output_layer( (1+NPINT*(IDAY-1)+(N-1)), 9, : ) = INFLPI(1:NLAYER) / DTP
output_layer( (1+NPINT*(IDAY-1)+(N-1)), 10, : ) = BYFLPI(1:NLAYER) / DTP
output_layer( (1+NPINT*(IDAY-1)+(N-1)), 11, : ) = TRANPI(1:NLAYER) / DTP

output_layer( (1+NPINT*(IDAY-1)+(N-1)), 12, : ) = VRFLPI(1:NLAYER) / DTP
output_layer( (1+NPINT*(IDAY-1)+(N-1)), 13, : ) = DSFLPI(1:NLAYER) / DTP
output_layer( (1+NPINT*(IDAY-1)+(N-1)), 14, : ) = NTFLPI(1:NLAYER) / DTP

IF (IDAY .EQ. 1) THEN
  DO  i = 1, NLAYER
  dummy = FWETNES(-6.18d0,Par(1,i),iModel) ! Wetnes bei FK
    IF ((FTheta(dummy,Par(1,i),iModel) -  FTheta(WETC(I),Par(1,i),iModel)) .GT. 0) THEN
      AWATIFK(i) = (FTheta(dummy,Par(1,i),iModel) - FTheta(WETC(i),Par(1,i),iModel)) * SWATMX(i)/Par(1,i)
      !call dblpr("awati",-1, AWATIFK(i),1)
    END IF
  END DO
END IF

DO i = 1, NLAYER
    IF ((FTheta(WETNES(I),Par(1,i),iModel) -  FTheta(WETC(I),Par(1,i),iModel)) .GT. 0) THEN
      RELAWATI(i) = (FTheta(WETNES(i),Par(1,i),iModel) - FTheta(WETC(I),Par(1,i),iModel)) * &
      SWATMX(i)/Par(1,I) / AWATIFK(i)
    ELSE
      !call intpr("NO RELAWATI: ", -1,(/ I, YEAR, MONTH, DOM/),4)
      RELAWATI(i) = 0.0
    END IF
  END DO

output_layer( IDAY, 15, : ) = RELAWATI(1:NLAYER)
