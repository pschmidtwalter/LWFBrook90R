writeParam.in <- function(parameters, outmat, filename){
  
  # average duration of precipitation events by month, hours
  PDurs <- paste(parameters$pdur, collapse =  "\t")
  
  
  # construct character vectors to be written line by line
  lines.sec1 <-   c(
    "''",
    "''",
    "''",
    "''",
    "''",
    "''",
    paste(formatC(parameters$ndays,  format  =  "fg"),"''"),
    paste("0", "''"),
    paste("0","''"),
    paste("0    0","''"),
    paste("'","out\\","'", " ''", sep  =  ""),
    "''",
    "''")
  
  lines.sec2 <- c(
    "''",
    "''",
    paste(formatC(parameters$eslope,  format = "fg"), "''"),
    paste(formatC(parameters$aspect,   format = "fg"), "''"),
    paste(formatC(parameters$alb,   format = "fg"), "''"),
    paste(formatC(parameters$albsn,   format = "fg"), "''"),
    paste(formatC(parameters$c1,   format = "fg"), "''"),
    paste(formatC(parameters$c2,   format = "fg"), "''"),
    paste(formatC(parameters$c3,   format = "fg"), "''"),
    paste(formatC(parameters$wndrat,   format = "fg"), "''"),
    paste(formatC(parameters$fetch,   format = "fg"), "''"),
    paste(formatC(parameters$z0w,   format = "fg"), "''"),
    paste(formatC(parameters$zw,   format = "fg"), "''"),
    "''",
    "''",
    paste(formatC(parameters$lwidth,   format = "fg"), "''"),
    paste(formatC(parameters$obsheight*parameters$czs,   format = "fg"), "''"),
    paste(formatC(parameters$z0s,   format = "fg"), "''"),
    paste(formatC(parameters$lpc,   format = "fg"), "''"),
    paste(formatC(parameters$cs,   format = "fg"), "''"),
    paste(formatC(parameters$czs,   format = "fg"), "''"),
    paste(formatC(parameters$czr,   format = "fg"), "''"),
    paste(formatC(parameters$hs,   format = "fg"), "''"),
    paste(formatC(parameters$hr,   format = "fg"), "''"),
    paste(formatC(parameters$zminh,   format = "fg"), "''"),
    paste(formatC(parameters$rhotp,   format = "fg"), "''"),
    paste(formatC(parameters$nn,   format = "fg"), "''"),
    "''",
    "''",
    paste(formatC(parameters$rstemp,   format = "fg"), "''"),
    "''",
    PDurs,
    paste(formatC(parameters$intrainini,   format = "fg"), "''"),
    paste(formatC(parameters$intsnowini,   format = "fg"), "''"),
    paste(formatC(parameters$frintlai,   format = "fg"), "''"),
    paste(formatC(parameters$fsintlai,   format = "fg"), "''"),
    paste(formatC(parameters$frintsai,   format = "fg"), "''"),
    paste(formatC(parameters$fsintsai,   format = "fg"), "''"),
    paste(formatC(parameters$cintrl,   format = "fg"), "''"),
    paste(formatC(parameters$cintrs,   format = "fg"), "''"),
    paste(formatC(parameters$cintsl,   format = "fg"), "''"),
    paste(formatC(parameters$cintss,   format = "fg"), "''"),
    "''",
    "''",
    paste(formatC(parameters$melfac,   format = "fg"), "''"),
    paste(formatC(parameters$ccfac,   format = "fg"), "''"),
    paste(formatC(parameters$laimlt,   format = "fg"), "''"),
    paste(formatC(parameters$saimlt,   format = "fg"), "''"),
    paste(formatC(parameters$grdmlt,   format = "fg"), "''"),
    paste(formatC(parameters$maxlqf,   format = "fg"), "''"),
    paste(formatC(parameters$ksnvp,   format = "fg"), "''"),
    paste(formatC(parameters$snoden,   format = "fg"), "''"),
    "''",
    "''",
    paste(formatC(parameters$glmax,   format = "fg"), "''"),
    paste(formatC(parameters$radex,   format = "fg"), "''"),
    paste(formatC(parameters$glmin,   format = "fg"), "''"),
    paste(formatC(parameters$rm,   format = "fg"), "''"),
    paste(formatC(parameters$r5,   format = "fg"), "''"),
    paste(formatC(parameters$cvpd,   format = "fg"), "''"),
    paste(formatC(parameters$tl,   format = "fg"), "''"),
    paste(formatC(parameters$t1,   format = "fg"), "''"),
    paste(formatC(parameters$t2,   format = "fg"), "''"),
    paste(formatC(parameters$th,   format = "fg"), "''"),
    "''",
    "''",
    paste(formatC(parameters$mxkpl,   format = "fg"), "''"),
    paste(formatC(parameters$maxrlen,   format = "fg"), "''"),
    paste(formatC(parameters$initrlen,   format = "fg"), "''"),
    paste(formatC(parameters$initrdep,   format = "fg"), "''"),
    paste(formatC(parameters$rgrorate,   format = "fg"), "''"),
    paste(formatC(parameters$rgroper,   format = "fg"), "''"),
    paste(formatC(parameters$fxylem,   format = "fg"), "''"),
    paste(formatC(parameters$psicr,   format = "fg"), "''"),
    paste(formatC(parameters$rrad,   format = "fg"), "''"),
    paste(formatC(parameters$nooutf,   format = "fg"), "''"),
    "''",
    "''",
    paste(formatC(nrow(parameters$soil_nodes),   format = "fg"), "''"),
    paste(formatC(nrow(parameters$soil_materials),   format = "fg"), "''"),
    paste(formatC(parameters$ilayer,   format = "fg"), "''"),
    paste(formatC(parameters$qlayer,   format = "fg"), "''"),
    paste(formatC(1,   format = "fg"), "''"),
    "''",
    "''",
    "''")
  # last section
  lines.sec3 <- c(
    "''",
    paste(formatC(parameters$rssa,   format = "fg"), "''"),
    paste(formatC(parameters$rssb,   format = "fg"), "''"),
    "''",
    "''",
    paste(formatC(parameters$infexp,   format = "fg"), "''"),
    paste(formatC(parameters$bypar,   format = "fg"), "''"),
    paste(formatC(parameters$qfpar,   format = "fg"), "''"),
    paste(formatC(parameters$qffc,   format = "fg"), "''"),
    paste(formatC(parameters$imperv,   format = "fg"), "''"),
    paste(formatC(parameters$dslope,   format = "fg"), "''"),
    paste(formatC(parameters$slopelen,   format = "fg"), "''"),
    paste(formatC(parameters$drain,   format = "fg"), "''"),
    paste(formatC(parameters$gsc,   format = "fg"), "''"),
    paste(formatC(parameters$gsp,   format = "fg"), "''"),
    "''",
    "''",
    paste(formatC(parameters$dtimax,   format = "fg"), "''"),
    paste(formatC(parameters$dswmax,   format = "fg"), "''"),
    paste(formatC(parameters$dpsimax,   format = "fg"), "''"),
    "''",
    "'End of in\\Param.in'")
  
  # Param.in schreiebn--------------------------------------------------------------------------------
  #open file for writing
  p.in <- file(filename, open  =  "wt")
  
  #lines.sec1
  writeLines(lines.sec1, p.in)
  
  #output-matrix
  writeLines(apply(cbind(paste0("'",row.names(outmat),"'"), format(outmat)), 1, paste,
                   collapse = "\t"), p.in)
  
  #lines.sec2
  writeLines(lines.sec2, p.in)
  
  #materials-table
  writeLines(apply(format(parameters$soil_materials), 1, paste, collapse = "\t"), p.in)
  writeLines(c("''","''"), p.in)
  
  #write Templayers
  writeLines(apply(format(parameters$soil_nodes[,c("layer","midpoint", "thick", "mat", "psiini", "rootden")]),
                   1, paste, collapse = "\t"), p.in)
  
  writeLines(lines.sec3, p.in)
  
  on.exit(close(p.in))
}