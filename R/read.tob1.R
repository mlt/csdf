#' Read Table Oriented Binary 1 file
#'
#' @param file the name of the file which the data are to be read from. It has
#'   to be a character string and cannot be a connection as mixed text & binary
#'   operations are involved.
#' @param digits Number of significant digits to round floating point values to.
#'   Helpful if \code{\linkS4class{csdf}} is to be written into text based
#'   format like TOA5. See \code{\link{signif}}. Use \code{NULL} or \code{NA} if
#'   no rounding is necessary.
#' @param endian Endianness for binary data. Should be always \code{"little"}.
#'
#' @return an S4 class \code{\linkS4class{csdf}}
#' @export
#' @importFrom stats setNames
#'
#' @examples
#' Sys.setenv(TZ='GMT')
#' fpath <- system.file("extdata", "TOB1_Station_Daily.dat", package="csdf")
#' obj <- read.tob1(fpath)
#' \dontrun{
#' write.toa5(obj, "somewhere.dat")
#' }
read.tob1 <- function(file, digits=6, endian="little") {
  meta <- utils::read.csv(file, FALSE, col.names=c('type', meta.names), nrows=1)
  stopifnot(meta[1, "type"] == "TOB1")
  file.text <- file(file, "r")
  header <- utils::read.csv(file.text, TRUE, row.names=c(variables.row.names, "type"),
                            nrows=3, skip = 1, check.names = FALSE)
  if (all(c("SECONDS", "NANOSECONDS") == names(header)[1:2]))
    header <- data.frame(TIMESTAMP=c("TS", "", "SecNano"),
                         header[, -(1:2)])
  pos <- seek(file.text)
  file.bin <- file(file, "rb")
  seek(file.bin, pos)
  repeat {
    row <- lapply(setNames(nm=names(header)), function(t) {
      switch(as.character(header['type', t]),
             ULONG=readBin(file.bin, integer(), size=4, endian=endian),
             IEEE4={
               fp <- readBin(file.bin, double(), size=4, endian=endian)
               ifelse(is.numeric(digits), signif(fp, 7), fp)
             },
             SecNano={
               secs <- readBin(file.bin, integer(), size=4, endian=endian)
               nano <- readBin(file.bin, integer(), size=4, endian=endian)
               as.POSIXct(secs + nano/1e9, "UTC", origin="1990-01-01")
             },
             # TODO: IEEE8, FP2, LONG, BOOL, and ASCII(len)
             stop(sprintf("Unsupported data type: %s", t))
      )
    })
    if (length(row[[1]]) == 0)
      break
    # TODO: preallocate data.frame based on total fields size and file size
    if(exists("dat", inherits = FALSE))
      dat[nrow(dat)+1,] <- as.data.frame(row)
    else
      dat <- as.data.frame(row)
  }
  new("csdf", data=dat, variables=header[-3,], meta=meta[,-1])
}
