
output_layer( (1+NPINT*(IDAY-1)+(N-1)), 1, : ) = YY
output_layer( (1+NPINT*(IDAY-1)+(N-1)), 2, : ) = MM
output_layer( (1+NPINT*(IDAY-1)+(N-1)), 3, : ) = DD
output_layer( (1+NPINT*(IDAY-1)+(N-1)), 4, : ) = DOY

output_layer( (1+NPINT*(IDAY-1)+(N-1)), 5, : ) = SWATI(1:NLAYER)
output_layer( (1+NPINT*(IDAY-1)+(N-1)), 6, : ) = THETA(1:NLAYER)
output_layer( (1+NPINT*(IDAY-1)+(N-1)), 7, : ) = WETNES(1:NLAYER)
output_layer( (1+NPINT*(IDAY-1)+(N-1)), 8, : ) = max(-9999.d0, PSIM(1:NLAYER))
output_layer( (1+NPINT*(IDAY-1)+(N-1)), 9, : ) = max(-9999.d0, PSITI(1:NLAYER))

output_layer( (1+NPINT*(IDAY-1)+(N-1)), 10, : ) = INFLPI(1:NLAYER)
output_layer( (1+NPINT*(IDAY-1)+(N-1)), 11, : ) = BYFLPI(1:NLAYER)
output_layer( (1+NPINT*(IDAY-1)+(N-1)), 12, : ) = TRANPI(1:NLAYER)
output_layer( (1+NPINT*(IDAY-1)+(N-1)), 13, 1 ) = SLVP
output_layer( (1+NPINT*(IDAY-1)+(N-1)), 13, 2:NLAYER ) = 0.d0
output_layer( (1+NPINT*(IDAY-1)+(N-1)), 14, : ) = VRFLPI(1:NLAYER)
output_layer( (1+NPINT*(IDAY-1)+(N-1)), 15, : ) = DSFLPI(1:NLAYER)
output_layer( (1+NPINT*(IDAY-1)+(N-1)), 16, : ) = NTFLPI(1:NLAYER)
