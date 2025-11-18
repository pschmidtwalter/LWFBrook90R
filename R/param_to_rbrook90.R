#' Create parameter vectors for the LWF-Brook90 Fortran library
#'
#' Functions to create the \code{param} and \code{siteparam} vectors to be passed to the C-interface.
#'
#'
#' @param b90f_input A named list of model parameters.
#'
#' @return A numeric vector with the parameters in the required order.
#'
#' @name parms_to_rbrook90
#'
#' @rdname parms_to_rbrook90
#' @export
param_to_rlwfbrook90 <- function(b90f_input){

  required_nms <- c("ndays","eslope","aspect","alb","albsn","c1","c2","c3","wndrat",
                    "fetch","z0w","zw","lwidth","obsheight","czs","z0s","lpc","cs",
                    "czs","czr","hs","hr","zminh","rhotp","nn","rstemp","intrainini",
                    "intsnowini","frintlai","fsintlai","frintsai","fsintsai","cintrl",
                    "cintrs","cintsl","cintss","melfac","ccfac","laimlt","saimlt",
                    "grdmlt","maxlqf","ksnvp","snoden","glmax","radex","glmin","rm",
                    "r5","cvpd","tl","t1","t2","th","mxkpl","maxrlen","initrlen",
                    "initrdep","rgrorate","rgroper","fxylem","psicr","rrad","nooutf",
                    "soil_nodes","soil_materials","ilayer","qlayer","imodel","rssa",
                    "rssb","infexp","bypar","qfpar","qffc","imperv","dslope","slopelen",
                    "drain","gsc","gsp","dtimax","dswmax","dpsimax")

  if (anyNA(b90f_input[required_nms])) {
    stop(paste("b90f_input-list for r_lwfbrook90 is incomplete. \nNA list items:",
               paste0(names(b90f_input[required_nms][is.na(b90f_input[required_nms])  ]),
                      collapse = ", ")))
  }
  if (any(!required_nms %in% names(b90f_input))) {
    stop(paste("b90f_input-list for r_lwfbrook90 is incomplete. \nMissing list items:",
               paste0(required_nms[!required_nms %in% names(b90f_input[required_nms])],
                      collapse = ", ")))
  }

  fparms <- c(b90f_input$ndays,
              0, # heat: always 0, it doesnt work.
              b90f_input$eslope,
              b90f_input$aspect,
              b90f_input$alb,
              b90f_input$albsn,
              b90f_input$c1,
              b90f_input$c2,
              b90f_input$c3,
              b90f_input$wndrat,
              b90f_input$fetch,
              b90f_input$z0w,
              b90f_input$zw,
              b90f_input$lwidth,
              b90f_input$obsheight*b90f_input$czs,   # as calculated in MS Access-GUI
              b90f_input$z0s,
              b90f_input$lpc,
              b90f_input$cs,
              b90f_input$czs,
              b90f_input$czr,
              b90f_input$hs,
              b90f_input$hr,
              b90f_input$zminh,
              b90f_input$rhotp,
              b90f_input$nn,
              b90f_input$rstemp,
              b90f_input$intrainini,
              b90f_input$intsnowini,
              b90f_input$frintlai,
              b90f_input$fsintlai,
              b90f_input$frintsai,
              b90f_input$fsintsai,
              b90f_input$cintrl,
              b90f_input$cintrs,
              b90f_input$cintsl,
              b90f_input$cintss,
              b90f_input$melfac,
              b90f_input$ccfac,
              b90f_input$laimlt,
              b90f_input$saimlt,
              b90f_input$grdmlt,
              b90f_input$maxlqf,
              b90f_input$ksnvp,
              b90f_input$snoden,
              b90f_input$glmax,
              b90f_input$radex,
              b90f_input$glmin,
              b90f_input$rm,
              b90f_input$r5,
              b90f_input$cvpd,
              b90f_input$tl,
              b90f_input$t1,
              b90f_input$t2,
              b90f_input$th,
              b90f_input$mxkpl,
              b90f_input$maxrlen,
              b90f_input$initrlen,
              b90f_input$initrdep,
              b90f_input$rgrorate,
              b90f_input$rgroper,
              b90f_input$fxylem,
              b90f_input$psicr,
              b90f_input$rrad,
              b90f_input$nooutf,
              nrow(b90f_input$soil_nodes),
              nrow(b90f_input$soil_materials),
              b90f_input$ilayer,
              b90f_input$qlayer,
              ifelse(b90f_input$imodel == "MvG", 1,0),
              b90f_input$rssa,
              b90f_input$rssb,
              b90f_input$infexp,
              b90f_input$bypar,
              b90f_input$qfpar,
              b90f_input$qffc,
              b90f_input$imperv,
              b90f_input$dslope,
              b90f_input$slopelen,
              b90f_input$drain,
              b90f_input$gsc,
              b90f_input$gsp,
              b90f_input$dtimax,
              b90f_input$dswmax,
              b90f_input$dpsimax)

  return(fparms)
}

#' @rdname parms_to_rbrook90
#' @export
siteparam_to_rlwfbrook90 <- function(b90f_input){

  required_nms <- c("startdate", "coords_y", "snowini","gwatini", "prec_interval",
                    "snowlqini","snowccini", "water_table_depth")

  if (anyNA(b90f_input[required_nms])) {
    stop(paste("b90f_input-list for r_lwfbrook90 is incomplete. \nNA list items:",
               paste0(names(b90f_input[required_nms][is.na(b90f_input[required_nms])  ]),
                      collapse = ", ")))
  }
  if (any(!required_nms %in% names(b90f_input))) {
    stop(paste("b90f_input-list for r_lwfbrook90 is incomplete. \nMissing list items:",
               paste0(required_nms[!required_nms %in% names(b90f_input[required_nms])],
                      collapse = ", ")))
  }

  # site param to be returned depends on wether a timeseries of water_table_depths is contained or not
  if (data.table::is.data.table(b90f_input$water_table_depth)) {
    fsiteparms <- with(b90f_input,
                       c(data.table::year(startdate), data.table::yday(startdate),
                         coords_y, snowini, gwatini,
                         prec_interval, snowlqini,snowccini, -9999))
  } else {
    fsiteparms <- with(b90f_input,
                       c(data.table::year(startdate), data.table::yday(startdate),
                         coords_y, snowini, gwatini,
                         prec_interval, snowlqini,snowccini, water_table_depth))

  }

  return(fsiteparms)
}
