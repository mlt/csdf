#' Campbell Scientific Data Formats
#'
#' Read and write Campbell Scientific data formats like TOA5.
#'
#' \tabular{ll}{ Package: \tab csdf\cr Type: \tab Package\cr Version: \tab
#' 0.1\cr Date: \tab 2016-08-03\cr License: \tab AGPL (>= 3)\cr LazyLoad: \tab
#' yes\cr }
#'
#' \strong{T}able \strong{O}riented \strong{A}scii \strong{5} is one of the
#' formats available with LoggerNet software from Cambell Scientific when
#' exporting data out of their data loggers.
#'
#' If the only thing you want is to get your data in, look for
#' \code{\link{read.toa5}}. Make sure to set fixed time zone to avoid
#' unnecessary conversions.
#'
#' A convenience functions are provided to get a \code{\link{data.frame}} with
#' your data from \code{\link{slot}} \emph{data} using \code{\link{as}} or an S3
#' \code{\link{as.data.frame.csdf}} for \code{\link{as.data.frame}} generic.
#'
#' A quick plotting specialization is provided with
#' \code{\link{plot,csdf,missing-method}}. This requires some extra packages.
#'
#' @docType package
#' @name csdf-package
#' @importFrom utils read.csv write.table write.csv
#' @importFrom methods setClass new as
NULL
