% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/process_outputs.R
\name{process_outputs_LWFB90}
\alias{process_outputs_LWFB90}
\title{Aggregate and group model outputs similar to ancient LWFB90 textfile outputs (.ASC-files)}
\usage{
process_outputs_LWFB90(x, selection = set_outputLWFB90(), prec_interval = NULL)
}
\arguments{
\item{x}{Named list with items \code{x$output} and/or \code{x$layer_output}
(e.g. as returned by \code{\link{run_LWFB90}})}

\item{selection}{A [7,5]-matrix with row and column names, flagging the
desired groups of variables at specified time intervals (see
\code{\link{set_outputLWFB90}}).}

\item{prec_interval}{The precipitation interval of the simulation
that produced \code{x}. If available, the value \code{x$model_input$options_b90$prec_interval} is used.}
}
\value{
A named list containing the selected groups of variables in the
  desired temporal resolution. The names are constructed from
  \code{selection}'s row names and column names, suffixed by '.ASC' as a
  reminiscence to the former text file output of LWF-Brook90.
}
\description{
Returns selected groups of variables in the chosen temporal aggregation
}
\examples{
data("slb1_soil")
data("slb1_meteo")
opts <- set_optionsLWFB90(startdate = as.Date("2002-06-01"), enddate = as.Date("2002-06-05"))
parms <- set_paramLWFB90()
soil <- cbind(slb1_soil, hydpar_wessolek_tab(texture = slb1_soil$texture))

outsel <- set_outputLWFB90()
outsel[,] <- 1L

res <- run_LWFB90(options_b90 = opts,
           param_b90 = parms,
           climate = slb1_meteo,
           soil = soil)

# Calculate output-aggregations using the returned object
process_outputs_LWFB90(res, selection = outsel)

# or calculate aggregations at run time by passing the function via output_fun-arg
run_LWFB90(options_b90 = opts,
           param_b90 = parms,
           climate = slb1_meteo,
           soil = soil,
           rtrn_input = FALSE,
           output_fun = process_outputs_LWFB90,
           selection = outsel)$output_fun

}
