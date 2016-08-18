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
  file.length <- file.size(file)
  file.text <- file(file, "r")
  meta <- utils::read.csv(file.text, FALSE, col.names=c('type', meta.names), nrows=1)
  stopifnot(meta[1, "type"] == "TOB1")
  header <- utils::read.csv(file.text, TRUE, row.names=c(variables.row.names, "type"),
                            nrows=3, check.names = FALSE, stringsAsFactors=FALSE)
  if (all(c("SECONDS", "NANOSECONDS") == names(header)[1:2]))
    header <- data.frame(TIMESTAMP=c("TS", "", "SecNano"),
                         header[, -(1:2)],
                         stringsAsFactors=FALSE)
  sizes <- data.frame(type=c("LONG", "ULONG", "IEEE4", "IEEE8", "SecNano", "BOOL"),
                      length=c(4, 4, 4, 8, 8, 1), stringsAsFactors=FALSE)
  row.length <- with(merge(data.frame(type=as.character(header['type',])), sizes),
                     sum(length))
  pos <- seek(file.text)
  file.bin <- file(file, "rb")
  seek(file.bin, pos)
  nrow.max <- (file.length-pos)/row.length
  i <- 1
  repeat {
    row <- lapply(setNames(nm=names(header)), function(t) {
      switch(as.character(header['type', t]),
             ULONG=readBin(file.bin, integer(), size=4, endian=endian),
             LONG=readBin(file.bin, integer(), size=4, endian=endian),
             IEEE4={
               fp <- readBin(file.bin, double(), size=4, endian=endian)
               ifelse(is.numeric(digits), signif(fp, 7), fp)
             },
             IEEE8={
               fp <- readBin(file.bin, double(), size=8, endian=endian)
               ifelse(is.numeric(digits), signif(fp, 7), fp)
             },
             BOOL=readBin(file.bin, logical(), size=1),
             SecNano={
               secs <- readBin(file.bin, integer(), size=4, endian=endian)
               nano <- readBin(file.bin, integer(), size=4, endian=endian)
               as.POSIXct(secs + nano/1e9, "UTC", origin="1990-01-01")
             },
             # TODO: FP2 and ASCII(len)
             stop(sprintf("Unsupported data type: %s", t))
      )
    })
    if (length(row[[1]]) == 0)
      break
    # TODO: preallocate data.frame outside of loop completely
    # based on fields types
    if(exists("dat", inherits = FALSE))
      dat[i,] <- as.data.frame(row)
    else {
      dat <- as.data.frame(row)
      dat[nrow.max, 1] <- NULL
    }
    i <- i+1
  }
  new("csdf", data=dat, variables=header[-3,], meta=meta[,-1])
}
