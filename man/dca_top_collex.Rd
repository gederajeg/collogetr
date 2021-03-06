% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/fye_dca.R
\name{dca_top_collex}
\alias{dca_top_collex}
\title{Retrieve the distinctive collocates/collexemes of construction/word X}
\usage{
dca_top_collex(dca_res, dist_for = NULL, top_n = 20L)
}
\arguments{
\item{dca_res}{The output of \code{\link{collex_fye_dca}}}

\item{dist_for}{Character vectors of names of the contrasted constructions, or either \code{"a"}/\code{"A"}, or \code{"b"}/\code{"B"}. This argument indicate the preference of the collexeme/collocates (i.e. whether to Construction A or B).
\code{Construction A} is the name for the \emph{second} column from the output of \code{\link{collex_fye_dca}}. \code{Construction B} is the name for the \emph{third} column from the output of \code{\link{collex_fye_dca}}.}

\item{top_n}{Integer for the top-n distinctive collocates/collexemes. The default is \code{20L}.}
}
\value{
A data frame of distinctive collocates/collexemes for construction X.
}
\description{
A helper function to extract distinctive collocates/collexemes for construction/word X from the output of \code{\link{collex_fye_dca}}.
}
\examples{
\dontrun{
# distinctive for CxN B
dca_top_collex(dca_res, "b")

# distinctive for CxN A
dca_top_collex(dca_res, "a")
}
}
