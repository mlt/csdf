meta.names <- c("type", "station", "model", "serial", "os", "program", "signature", "table")
variables.row.names <- c("units", "pi")

#' Check csdf object validity
#'
#' Valid object has same column names for a \code{\link{data.frame}} in
#' \code{\link{slot}} \emph{data} as that in \code{\link{slot}}
#' \emph{variables}. The \code{\link{data.frame}} in the latter should have 2
#' rows: units and processing instruction used in CR Basic program.
#'
#' @param object the object to test
#'
#' @return TRUE if the object is a valid \code{\link{csdf-class}} object
validObject.csdf <- function(object) {
  all(
    names(object@data) == names(object@variables),
    row.names(object@variables) == variables.row.names,
    names(object@meta) == meta.names,
    nrow(object@meta) == 1L
  )
}


#' Class encapsulating content of Campbell Scientific data file
#'
#' Objects of this class are typically produced by \code{\link{read.toa5}}
#' function.
#'
#' @slot data \code{\link{data.frame}} with captured data.
#' @slot variables \code{\link{data.frame}} with rows units and processing
#'   instruction (Smp, Avg, Min, Max and such).
#' @slot meta \code{\link{data.frame}} with auxiliary information.
#
# @name csdf-class
csdf <- methods::setClass("csdf",
                          slots=list(data="data.frame", variables="data.frame", meta="data.frame"),
                          validity=validObject.csdf)

setAs("csdf", "data.frame", function(from) from@data)

#' S3 method to get data.frame
#'
#' @param x a csdf object
#' @param ... ignored
#'
#' @return \code{\link{data.frame}} from \code{\link{slot}} \emph{data}.
#' @export
#'
#' @examples
#' Sys.setenv(TZ='GMT')
#' fpath <- system.file("extdata", "Station_Daily.dat", package="csdf")
#' obj <- read.toa5(fpath)
#' summary(as.data.frame(obj))
as.data.frame.csdf <- function(x, ...) as(x, "data.frame")
