% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/results_summary_f.R
\name{extract_psa_result}
\alias{extract_psa_result}
\title{Extract PSA results from a treatment}
\usage{
extract_psa_result(x, element)
}
\arguments{
\item{x}{The output_sim data frame from the list object returned by \code{run_sim()}}

\item{element}{Variable for which PSA results are being extracted (single string)}
}
\value{
A dataframe with PSA results from the specified intervention
}
\description{
Extract PSA results from a treatment
}
\examples{
\dontrun{
extract_psa_result(results[[1]],"total_costs")
}
}
