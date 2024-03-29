% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/minmax.R
\name{minmax}
\alias{minmax}
\title{Minimum and maximum}
\usage{
minmax(x, empty_result = NULL, nThread = getOption("hutilscpp.nThread", 1L))
}
\arguments{
\item{x}{An atomic vector.}

\item{empty_result}{What should be returned when \code{length(x) == 0}?}

\item{nThread}{Number of threads to be used.}
}
\value{
Vector of two elements, the minimum and maximum of \code{x}, or \code{NULL}.
}
\description{
Minimum and maximum
}
