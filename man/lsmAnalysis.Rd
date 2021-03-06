% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/RCB_SupportFunctions.R
\name{lsmAnalysis}
\alias{lsmAnalysis}
\title{Computes the LS means analysis for model P1, P2 and P4}
\usage{
lsmAnalysis(asreml.obj, data, alpha)
}
\arguments{
\item{asreml.obj}{A fitted ASReml object}

\item{data}{A dataframe that contains the data used to create the ASReml object}

\item{alpha}{A number between 0 and 1 specifying the confidence level}
}
\value{
A dataframe containing the Entry, Yield, degree of freedom for t-test, SE, total number of observation for the entry after excluding missing data, upper and lower confidence intervals and mean separation grouping
}
\description{
Computes the LS means analysis for model P1, P2 and P4
}
