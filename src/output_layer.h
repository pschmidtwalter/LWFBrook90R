! 2020.04.20 Daily output directly to R.
! developed by V. Trotsiuk volodymyr.trostiuk@wsl.ch

output_layer( IDAY, 1, : ) = YY
output_layer( IDAY, 2, : ) = MM
output_layer( IDAY, 3, : ) = DD
output_layer( IDAY, 4, : ) = DOY

output_layer( IDAY, 5, : ) = SWATI(1:NLAYER)
output_layer( IDAY, 6, : ) = THETA(1:NLAYER)
output_layer( IDAY, 7, : ) = WETNES(1:NLAYER)
output_layer( IDAY, 8, : ) = max(-9999.d0, PSIM(1:NLAYER))
output_layer( IDAY, 9, : ) = max(-9999.d0, PSITI(1:NLAYER))

output_layer( IDAY, 10, : ) = INFLDI(1:NLAYER)
output_layer( IDAY, 11, : ) = BYFLDI(1:NLAYER)
output_layer( IDAY, 12, : ) = TRANDI(1:NLAYER)
output_layer( IDAY, 13, 1 ) = SLVPD
output_layer( IDAY, 13, 2:NLAYER ) = 0.d0
output_layer( IDAY, 14, : ) = VRFLDI(1:NLAYER)
output_layer( IDAY, 15, : ) = DSFLDI(1:NLAYER)
output_layer( IDAY, 16, : ) = NTFLDI(1:NLAYER)
