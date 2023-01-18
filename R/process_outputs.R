#' Aggregate and group model outputs similar to ancient LWFB90 textfile outputs (.ASC-files)
#'
#' Returns selected groups of variables in the chosen temporal aggregation
#'
#' @param x Named list with items \code{x$output} and/or \code{x$layer_output}
#'   (e.g. as returned by \code{\link{run_LWFB90}})
#' @param selection A [7,5]-matrix with row and column names, flagging the
#'   desired groups of variables at specified time intervals (see
#'   \code{\link{set_outputLWFB90}}).
#' @param prec_interval The precipitation interval of the simulation
#'   that produced \code{x}. If available, the value \code{x$model_input$options_b90$prec_interval} is used.
#'
#' @return A named list containing the selected groups of variables in the
#'   desired temporal resolution. The names are constructed from
#'   \code{selection}'s row names and column names, suffixed by '.ASC' as a
#'   reminiscence to the former text file output of LWF-Brook90.
#' @export
#' @examples
#' data("slb1_soil")
#' data("slb1_meteo")
#' opts <- set_optionsLWFB90(startdate = as.Date("2002-06-01"), enddate = as.Date("2002-06-05"))
#' parms <- set_paramLWFB90()
#' soil <- cbind(slb1_soil, hydpar_wessolek_tab(texture = slb1_soil$texture))
#'
#' outsel <- set_outputLWFB90()
#' outsel[,] <- 1L
#'
#' res <- run_LWFB90(options_b90 = opts,
#'            param_b90 = parms,
#'            climate = slb1_meteo,
#'            soil = soil)
#'
#' # Calculate output-aggregations using the returned object
#' process_outputs_LWFB90(res, selection = outsel)
#'
#' # or calculate aggregations at run time by passing the function via output_fun-arg
#' run_LWFB90(options_b90 = opts,
#'            param_b90 = parms,
#'            climate = slb1_meteo,
#'            soil = soil,
#'            rtrn_input = FALSE,
#'            output_fun = process_outputs_LWFB90,
#'            selection = outsel)$output_fun
#'
process_outputs_LWFB90 <- function(x,
                                   selection = set_outputLWFB90(),
                                   prec_interval = NULL)
{

  # to pass CRAN check notes
  adef<-NULL;awat<-NULL;balerr<-NULL;da<-NULL;doy<-NULL;evap<-NULL;flow<-NULL;gwat<-NULL;
  intr<-NULL;ints<-NULL;mo<-NULL;nits<-NULL;nl<-NULL;relawat<-NULL;rfal<-NULL;safrac<-NULL;
  seep<-NULL;sfal<-NULL;snow<-NULL;stres<-NULL;swat<-NULL;vrfln<-NULL;yr<-NULL;

  if (!is.null(x$model_input$options_b90$prec_interval)){
    prec_interval <- x$model_input$options_b90$prec_interval
  } else {
    if (is.null(prec_interval)){
      stop("Please provide the precipitation interval!")
    }
  }

  select <- rownames(selection)[which(rowSums(selection) > 0)]

  if (any(select == "Budg")) {
    Budg <- x$output[,c("yr","mo","da","doy","rfal","sfal","flow", "evap", "seep","snow","swat","gwat","intr","ints")]}
  if (any(select == "Flow")){
    Flow <- x$output[,c("yr","mo","da","doy","flow","seep","srfl","slfl","byfl","dsfl","gwfl","vrfln")]}
  if (any(select == "Evap")){
    Evap <- x$output[,c("yr","mo","da","doy","flow","evap","tran","irvp","isvp","slvp","snvp","pint","ptran","pslvp")]}
  if (any(select == "Abov")){
    Abov <- x$output[,c("yr","mo","da","doy","rfal","rint","sfal","sint","rthr","sthr","rsno","rnet","smlt","slfl","srfl")]}
  if (any(select == "Belo")){
    Belo <- x$layer_output[,c("yr","mo","da","doy","nl","infl","byfl","tran","vrfl","dsfl","ntfl")]}
  if (any(select == "Swat")){
    Swat <- x$layer_output[,c("yr","mo","da","doy","nl","swati","theta","wetnes","psimi")]}
  if (any(select == "Misc")){
    Misc <- x$output[,c("yr","mo","da","doy","vrfln","safrac","stres","adef","awat","relawat","nits","balerr")]}

  moutputs <- list() # results collection

  for (sel in select) {

    X <- get(sel) # get selected object

    if (sel  %in% c("Flow", "Evap", "Abov")) {
      for (per in rev(colnames(selection)[which(selection[sel,] == 1)])) {
        if (per == "Pre") {
          moutputs[[paste0(toupper(sel),"PRE.ASC")]] <-
            X[,lapply(.SD,function(x) {
              round(x / prec_interval, digits = 1)})]
        }

        if (per == "Day") {
          moutputs[[paste0(toupper(sel),"DAY.ASC")]] <-
            X[,lapply(.SD,function(x) {
              round(sum(x) / prec_interval, digits = 1)}),
              by = list(yr, mo, da, doy)]
        }
        if (per == "Mon") {
          moutputs[[paste0(toupper(sel),"MON.ASC")]] <-
            X[,lapply(.SD,function(x) {
              round(sum(x) / prec_interval, digits = 1)}),
              .SDcols = -c("da", "doy"),
              by = list(yr, mo)]
        }

        if (per == "Ann") {
          moutputs[[paste0(toupper(sel),"ANN.ASC")]] <-
            X[,lapply(.SD,function(x) {
              round(sum(x) / prec_interval, digits = 1)}),
              .SDcols = -c("mo","da", "doy"),
              by = yr]
        }


        if (per == "Pre") {
          moutputs[[paste0(toupper(sel),"PRE.ASC")]] <- X[,lapply(.SD, round, 1), by = list(yr, mo, da, doy)]
        }

        if (per == "Day") {
          moutputs[[paste0(toupper(sel),"DAY.ASC")]] <- X[,lapply(.SD, round, 1), by = list(yr, mo, da, doy)]
        }
        if (per == "Mon") {
          moutputs[[paste0(toupper(sel),"MON.ASC")]] <- X[,lapply(.SD, function(x) {round(sum(x),1)}),
                                                          .SDcols = -c("da","doy"), by = list(yr, mo)]
        }
        if (per == "Ann") {
          moutputs[[paste0(toupper(sel),"ANN.ASC")]] <- X[,lapply(.SD, function(x) {round(sum(x),1)}),
                                                          .SDcols = -c("mo","da","doy"),by = yr]
        }
      }
    }

    if (sel  == "Swat") {
      for (per in rev(colnames(selection)[which(selection[sel,] == 1)])) {
        if (per == "Pre") {
          moutputs[[paste0(toupper(sel),"PRE.ASC")]] <- X[,lapply(
            .SD, round, digits = 3)]
        }

        if (per == "Day") {
          moutputs[[paste0(toupper(sel),"DAY.ASC")]] <-
            X[,lapply(.SD,function(x) {
              round(mean(x), digits = 3)}),
              by = list(yr, mo, da, doy, nl)]
        }
        if (per == "Mon") {
          moutputs[[paste0(toupper(sel),"MON.ASC")]] <-
            X[,lapply(.SD,function(x) {
              round(mean(x), digits = 3)}),
              .SDcols = -c("da", "doy"),
              by = list(yr, mo,  nl)]
        }

        if (per == "Ann") {
          moutputs[[paste0(toupper(sel),"ANN.ASC")]] <-
            X[,lapply(.SD,function(x) {
              round(mean(x), digits = 3)}),
              .SDcols = -c("mo","da", "doy"),
              by = list(yr,   nl)]
        }
      }
    }
    if (sel  == "Belo") {
      for (per in rev(colnames(selection)[which(selection[sel,] == 1)])) {
        if (per == "Pre") {
          moutputs[[paste0(toupper(sel),"PRE.ASC")]] <-
            X[,lapply(.SD,function(x) {
              round(x / prec_interval, digits = 1)})]
        }
        if (per == "Day") {
          moutputs[[paste0(toupper(sel),"DAY.ASC")]] <-
            X[, lapply(.SD,function(x) {
              round(sum(x) / prec_interval, digits = 1)}),
              by = list(yr, mo, da, doy, nl)]

        }
        if (per == "Mon") {
          moutputs[[paste0(toupper(sel),"MON.ASC")]] <-
            X[,lapply(.SD, function(x) {
              round(sum(x) / prec_interval, digits = 1)}),
              by = list(yr, mo, nl)]
        }
        if (per == "Ann") {
          moutputs[[paste0(toupper(sel),"ANN.ASC")]] <-
            X[,lapply(.SD,function(x) {
              round(sum(x) / prec_interval, digits = 1)}),
              by = list(yr, nl)]
        }
      }
    }
    if (sel  == "Budg") {
      for (per in rev(colnames(selection)[which(selection[sel,] == 1)])) {
        if (per == "Pre") {
          moutputs[[paste0(toupper(sel),"PRE.ASC")]] <-
            # for fluxes: divide by `prec_interval`
            X[,list(yr, mo, da, doy,
                    prec = round( (rfal+sfal) / prec_interval, 1),
                    flow = round(flow / prec_interval,1),
                    evap = round(evap / prec_interval,1),
                    seep = round(seep / prec_interval,1),
                    # for state variables: take last entry in period
                    snow = round(snow,1),
                    swat = round(swat,1),
                    gwat = round(gwat,1),
                    intr = round(intr,1),
                    ints = round(ints,1))]
        }

        if (per == "Day") {
          moutputs[[paste0(toupper(sel),"DAY.ASC")]] <-
            X[, list(
              prec = round(sum(rfal  +sfal) / prec_interval,1),
              flow = round(sum(flow) / prec_interval,1),
              evap = round(sum(evap) / prec_interval,1),
              seep = round(sum(seep) / prec_interval,1),
              # for state variables: take last entry in period
              snow = round(last(snow),1),
              swat = round(last(swat),1),
              gwat = round(last(gwat),1),
              intr = round(last(intr),1),
              ints = round(last(ints),1)),
              by = list(yr, mo, da, doy)]
        }
        if (per == "Mon") {
          moutputs[[paste0(toupper(sel),"MON.ASC")]] <-
            X[, list(
              prec = round(sum(rfal+sfal) / prec_interval,1),
              flow = round(sum(flow) / prec_interval,1),
              evap = round(sum(evap) / prec_interval,1),
              seep = round(sum(seep) / prec_interval,1),
              # for state variables: take last entry in period
              snow = round(last(snow),1),
              swat = round(last(swat),1),
              gwat = round(last(gwat),1),
              intr = round(last(intr),1),
              ints = round(last(ints),1)),
              by = list(yr, mo)]
        }
        if (per == "Ann") {
          moutputs[[paste0(toupper(sel),"ANN.ASC")]] <-
            X[, list(
              prec = round(sum(rfal+sfal) / prec_interval,1),
              flow = round(sum(flow) / prec_interval,1),
              evap = round(sum(evap) / prec_interval,1),
              seep = round(sum(seep) / prec_interval,1),
              # for state variables: take last entry in period
              snow = round(last(snow),1),
              swat = round(last(swat),1),
              gwat = round(last(gwat),1),
              intr = round(last(intr),1),
              ints = round(last(ints),1)),
              by = list(yr)]
        }
      }
    }
    if (sel  == "Misc") {
      for (per in rev(colnames(selection)[which(selection[sel,] == 1)])) {
        if (per == "Pre") {
          moutputs[[paste0(toupper(sel),"PRE.ASC")]] <-
            X[, list(yr, mo, da, doy, pint = rowid(yr, doy),
                     vrfln   = round(vrfln / prec_interval,1),
                     safrac  = round(safrac,3),
                     adef    = round(adef,3),
                     awat    = round(awat,1),
                     relawat = round(relawat,3),
                     nits)]
        }

        if (per == "Day") {
          moutputs[[paste0(toupper(sel),"DAY.ASC")]] <-
            setcolorder(
              cbind(X[, list(vrfln   = round(sum(vrfln) / prec_interval,1),
                             safrac  = round(mean(safrac),3),
                             adef    = round(mean(adef),3),
                             awat    = round(mean(awat),1),
                             relawat = round(mean(relawat),3),
                             nits = sum(nits)),
                      by = list(yr, mo, da, doy)],
                    X[rowid(yr,doy) == prec_interval,
                      list(stres   = round(stres,3),
                           balerr  = round(balerr, 3)),
                      by = list(yr, mo, da, doy)][,list(balerr, stres)]),
              names(X))[]

        }
        if (per == "Mon") {
          moutputs[[paste0(toupper(sel),"MON.ASC")]] <-
            setcolorder(
              cbind(X[, list(vrfln   = round(sum(vrfln) / prec_interval,1),
                             safrac  = round(mean(safrac),3),
                             adef    = round(mean(adef),3),
                             awat    = round(mean(awat),1),
                             relawat = round(mean(relawat),3),
                             nits = sum(nits)),
                      by = list(yr, mo)],
                    X[rowid(yr,doy) == prec_interval,
                      list(stres   = round(mean(stres),3),
                           balerr  = round(sum(balerr), 3)),
                      by = list(yr, mo)][,list(balerr, stres)]),
              names(X)[-c(3,4)])[]

        }
        if (per == "Ann") {
          setcolorder(
            cbind(X[, list(vrfln   = round(sum(vrfln) / prec_interval,1),
                           safrac  = round(mean(safrac),3),
                           adef    = round(mean(adef),3),
                           awat    = round(mean(awat),1),
                           relawat = round(mean(relawat),3),
                           nits = sum(nits)),
                    by = list(yr)],
                  X[rowid(yr,doy) == prec_interval,
                    list(stres   = round(mean(stres),3),
                         balerr  = round(sum(balerr), 3)),
                    by = list(yr)][,list(balerr, stres)]),
            names(X)[-c(2,3,4)])[]
        }
      }
    }
  }


  return(moutputs)
}
