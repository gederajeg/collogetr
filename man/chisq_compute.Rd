% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/chisq.R
\name{chisq_compute}
\alias{chisq_compute}
\title{Chi-square contribution}
\usage{
chisq_compute(df, collstr_digit)
}
\arguments{
\item{df}{The output of \code{\link{assoc_prepare}}.}

\item{collstr_digit}{The floating digits of the collostruction strength. It is passed on from \code{\link{collex_chisq}} and the default is \code{3}.}
}
\value{
A double vector of collostruction strength based on chi-square contribution
}
\description{
Compute the chi-square contribution as the association measure of the co-occurrence cell (i.e., cell \code{a}).
    This is an internal function called via \code{\link{collex_chisq}}.
}
