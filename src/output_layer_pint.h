
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
