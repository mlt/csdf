#' Read TOA5 file
#'
#' This function reads CSV-alike TOA5 file. It assumes that \emph{TIMESTAMP}
#' column and those ending with \emph{_TMx} and \emph{_TMn} contain time stamps
#' in \strong{"YYYY-MM-DD HH:MM:SS"} format.
#'
#' It relies heavily on \code{\link{as.POSIXct}} for time stamp recognition as
#' used internally by \code{\link{read.table}}. Therefore it is important to
#' make sure correct time zone is set so you won't get unexpected conversions.
#' One approach is to use \code{Sys.setenv(TZ='GMT')} in the code. Use something
#' like this if you chose to stick to local time zone but decided to neglect DST
#' switch.
#'
#' No column name sanitation is performed. As a result, column names are likely
#' not syntactically valid if arrays were used in CR Basic. To reference such
#' columns, use backquote marks, e.g., \code{obj@data$`Air(1)`}.
#'
#' "NAN" is treated as NA.
#'
#' @param file the name of the file to read
#'
#' @return an S4 class \code{\linkS4class{csdf}}
#' @export
#'
#' @examples
#' Sys.setenv(TZ='GMT')
#' fpath <- system.file("extdata", "Station_Daily.dat", package="csdf")
#' obj <- read.toa5(fpath)
read.toa5 <- function(file) {
  meta <- utils::read.csv(file, FALSE, col.names=c('type', meta.names), nrows=1)
  stopifnot(meta[1, "type"] == "TOA5")
  header <- utils::read.csv(file, TRUE, row.names=variables.row.names,
                            nrows=2, skip = 1, check.names = FALSE)
  vars <- names(header)
  idx <- which(grepl("_TM[xn]|TIMESTAMP$", vars))
  len <- length(idx)
  colClasses <- `names<-`(rep("POSIXct", len), vars[idx])
  dat <- utils::read.csv(file, FALSE, skip = 4, stringsAsFactors=FALSE, na.strings = "NAN",
                  check.names = FALSE, col.names = vars, colClasses = colClasses)
  new("csdf", data=dat, variables=header, meta=meta[,-1])
}
