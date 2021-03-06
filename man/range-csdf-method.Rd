% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/range.R
\docType{methods}
\name{range,csdf-method}
\alias{range,csdf-method}
\title{Temporal coverage}
\usage{
\S4method{range}{csdf}(x)
}
\arguments{
\item{x}{csdf object to get temporal coverage of}
}
\value{
2 elements vector with min & max time stamps
}
\description{
Temporal coverage
}
\examples{
Sys.setenv(TZ='GMT')
fpath <- system.file("extdata", "Station_Daily.dat", package="csdf")
obj <- read.toa5(fpath)
range(obj)
}

