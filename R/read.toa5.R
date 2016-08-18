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
#' @param file the name of the file which the data are to be read from. If it
#'   does not contain an \emph{absolute} path, the file name is \emph{relative}
#'   to the current working directory, \code{\link{getwd}}(). Tilde-expansion is
#'   performed where supported. This can be a compressed file (see
#'   \code{\link{file}}).
#'
#'   Alternatively, \code{file} can be a readable text-mode
#'   \code{\link{connection}} (which will be opened for reading if necessary,
#'   and if so \code{\link{close}}d (and hence destroyed) at the end of the
#'   function call). (If \code{\link{stdin}}() is used, the prompts for lines
#'   may be somewhat confusing. Terminate input with a blank line or an
#'   \code{EOF} signal, \code{Ctrl-D} on Unix and \code{Ctrl-Z} on Windows. Any
#'   pushback on \code{stdin()} will be cleared before return.)
#'
#'   \code{file} can also be a complete URL. (For the supported URL schemes, see
#'   the 'URLs' section of the help for \code{\link{url}}.)
#' @param fileEncoding character string: if non-empty declares the encoding used
#'   on a file (not a connection) so the character data can be re-encoded. See
#'   \code{\link{read.table}} for more details.
#'
#' @return an S4 class \code{\linkS4class{csdf}}
#' @export
#'
#' @examples
#' Sys.setenv(TZ='GMT')
#' fpath <- system.file("extdata", "Station_Daily.dat", package="csdf")
#' obj <- read.toa5(fpath)
read.toa5 <- function(file, fileEncoding = "") {
  # taken from read.table
  if (is.character(file)) {
    file <- if (nzchar(fileEncoding))
      file(file, "rt", encoding = fileEncoding)
    else file(file, "rt")
    on.exit(close(file))
  }
  if (!inherits(file, "connection"))
    stop("'file' must be a character string or connection")
  if (!isOpen(file, "rt")) {
    open(file, "rt")
    on.exit(close(file))
  }

  meta <- utils::read.csv(file, FALSE, col.names=c('type', meta.names), nrows=1)
  stopifnot(meta[1, "type"] == "TOA5")
  header <- utils::read.csv(file, TRUE, row.names=variables.row.names,
                            nrows=2, check.names = FALSE)
  vars <- names(header)
  idx <- which(grepl("_TM[xn]|TIMESTAMP$", vars))
  len <- length(idx)
  colClasses <- `names<-`(rep("POSIXct", len), vars[idx])
  dat <- utils::read.csv(file, FALSE, stringsAsFactors=FALSE, na.strings = "NAN",
                  check.names = FALSE, col.names = vars, colClasses = colClasses)
  new("csdf", data=dat, variables=header, meta=meta[,-1])
}
