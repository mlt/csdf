#' Temporal coverage
#'
#' @param x csdf object to get temporal coverage of
#'
#' @return 2 elements vector with min & max time stamps
#' @export
#'
#' @examples
#' Sys.setenv(TZ='GMT')
#' fpath <- system.file("extdata", "Station_Daily.dat", package="csdf")
#' obj <- read.toa5(fpath)
#' range(obj)
setMethod("range", signature(x="csdf"), function(x) {
  range(x@data$TIMESTAMP)
})
