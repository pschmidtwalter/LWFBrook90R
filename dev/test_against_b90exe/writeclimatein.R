writeClimate.in <- function(climveg, parameters, 
                            filename = "in/Climate.in") {
  
  climveg$year <- as.integer(format(climveg$dates, "%Y"))
  climveg$month <- as.integer(format(climveg$dates, "%m"))
  climveg$mday <- as.integer(format(climveg$dates, "%d"))
  climveg$doy <- as.integer(format(climveg$dates, "%j"))
  
  #File Header
  ClimStr <- c(
    "'** First year First DOY  Latitude  Snow[mm]  GWat[mm]     NPINT'",
    
    paste("     ", climveg$year[1], climveg$doy[1], round(parameters$coords_y,2), parameters$snowini, parameters$gwatini, 1, sep = "       " )
  )
  
  #format Climate Table
  climveg$spaces <- " " #better output format with write.table
  
  climveg <- climveg[,c("spaces","year","month", "mday","globrad","tmax","tmin","vappres","wind","prec","mesfl","densef","height", "lai", "sai", "age")]
  names(climveg)[c(1,ncol(climveg))] <- c("'**","age'")
  
  # write Climate And VegetationTable (CLIMATE.IN)
  climate.in <- file(filename)
  writeLines(ClimStr, climate.in)
  close(climate.in)
  
  suppressWarnings(write.table(as.matrix(climveg),row.names = F,col.names = T, quote = F, sep = "\t",file = filename, append = TRUE))
  
  
}
