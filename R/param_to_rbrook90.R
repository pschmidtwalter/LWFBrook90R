#' Create a parameter vector for the r_lwfbrook90-function
#'
#' The \code{param} vector for \code{\link{r_lwfbrook90}} is created from model
#' parameters.
#'
#'
#' @param param_b90 A named list of model parameters.
#' @param imodel Name of hydraulic model ('MvG' or 'CH')
#'
#' @return A numerical vector with the parameters in the right order for
#'   \code{\link{r_lwfbrook90}}.
#' @export
param_to_rlwfbrook90 <- function(param_b90,
                                 imodel){

  fparms <- c(param_b90$ndays,
              0, # heat always 0, it doesnt work.
              param_b90$eslope,
              param_b90$aspect,
              param_b90$alb,
              param_b90$albsn,
              param_b90$c1,
              param_b90$c2,
              param_b90$c3,
              param_b90$wndrat,
              param_b90$fetch,
              param_b90$z0w,
              param_b90$zw,
              param_b90$lwidth,
              param_b90$obsheight*param_b90$czs,   # as calculated in MS Access-GUI
              param_b90$z0s,
              param_b90$lpc,
              param_b90$cs,
              param_b90$czs,
              param_b90$czr,
              param_b90$hs,
              param_b90$hr,
              param_b90$zminh,
              param_b90$rhotp,
              param_b90$nn,
              param_b90$rstemp,
              param_b90$intrainini,
              param_b90$intsnowini,
              param_b90$frintlai,
              param_b90$fsintlai,
              param_b90$frintsai,
              param_b90$fsintsai,
              param_b90$cintrl,
              param_b90$cintrs,
              param_b90$cintsl,
              param_b90$cintss,
              param_b90$melfac,
              param_b90$ccfac,
              param_b90$laimlt,
              param_b90$saimlt,
              param_b90$grdmlt,
              param_b90$maxlqf,
              param_b90$ksnvp,
              param_b90$snoden,
              param_b90$glmax,
              param_b90$radex,
              param_b90$glmin,
              param_b90$rm,
              param_b90$r5,
              param_b90$cvpd,
              param_b90$tl,
              param_b90$t1,
              param_b90$t2,
              param_b90$th,
              param_b90$mxkpl,
              param_b90$maxrlen,
              param_b90$initrlen,
              param_b90$initrdep,
              param_b90$rgrorate,
              param_b90$rgroper,
              param_b90$fxylem,
              param_b90$psicr,
              param_b90$rrad,
              param_b90$nooutf,
              nrow(param_b90$soil_nodes),
              nrow(param_b90$soil_materials),
              param_b90$ilayer,
              param_b90$qlayer,
              ifelse(imodel == "MvG", 1,0),
              param_b90$rssa,
              param_b90$rssb,
              param_b90$infexp,
              param_b90$bypar,
              param_b90$qfpar,
              param_b90$qffc,
              param_b90$imperv,
              param_b90$dslope,
              param_b90$slopelen,
              param_b90$drain,
              param_b90$gsc,
              param_b90$gsp,
              param_b90$dtimax,
              param_b90$dswmax,
              param_b90$dpsimax)

  return(fparms)
}
