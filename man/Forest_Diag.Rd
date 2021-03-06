% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/Forest_Diag.R
\name{Forest_Diag}
\alias{Forest_Diag}
\title{A forest plot for diagnostic accuracy studies}
\usage{
Forest_Diag(
  source_tbl,
  study,
  sort_var = NULL,
  group_var = NULL,
  disp_vars = NULL,
  sens_vars = c("sens", "sens.ci.lower", "sens.ci.upper"),
  spec_vars = c("spec", "spec.ci.lower", "spec.ci.upper"),
  weightmark = NULL
)
}
\arguments{
\item{source_tbl}{Dataframe containing the meta-analysis data}

\item{study}{Index column for unique study identifier e.g. "Obelix 50 B.c."}

\item{sort_var}{Column to sort rows by.}

\item{group_var}{Column variable containig group identifiers for subplots. Leave empty if not required.}

\item{disp_vars}{Column to display in tabulat part of the plot}

\item{sens_vars}{Columns to use for Sensitivity Forest Plot (median,lci,uci)}

\item{spec_vars}{Columns to use for Specificity Forest Plot (median,lci,uci)}

\item{weightmark}{column with values for weightings}
}
\description{
This function creates a six column table of the format "median ( LCI - UCI )". Called by Forest_Plot
}
