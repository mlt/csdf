#' Read Table Oriented Binary 1 file
#'
#' @param file a file name to read
#'
#' @return an S4 class \code{\linkS4class{csdf}}
#' @export
#'
#' @examples
#' Sys.setenv(TZ='GMT')
#' fpath <- system.file("extdata", "TOB1_Station_Daily.dat", package="csdf")
#' obj <- read.tob1(fpath)
#' \dontrun{
#' write.toa5(obj, "somewhere.dat)
#' }
read.tob1 <- function(file) {
  meta <- utils::read.csv(file, FALSE, col.names=c('type', meta.names), nrows=1)
  stopifnot(meta[1, "type"] == "TOB1")
  file.text <- file(file, "r")
  header <- utils::read.csv(file.text, TRUE, row.names=c(variables.row.names, "type"),
                            nrows=3, skip = 1, check.names = FALSE)
  vars <- names(header)
  dat <- data.frame()
  pos <- seek(file.text)
  file.bin <- file(file, "rb")
  seek(file.bin, pos)
  dat <- data.frame()
  repeat {
    row <- lapply(setNames(nm=names(header)), function(t) {
      switch(as.character(header['type', t]),
             ULONG=readBin(file.bin, integer(), size=4, endian="little"),
             IEEE4=signif( readBin(file.bin, double(), size=4, endian="little"), 7 ),
             SecNano={
               secs <- readBin(file.bin, integer(), size=4, endian="little")
               nano <- readBin(file.bin, integer(), size=4, endian="little")
               as.POSIXct(secs + nano/1e9, origin="1990-01-01")
             },
             # TODO: IEEE8, FP2, LONG, BOOL, and ASCII(len)
             stop(sprintf("Unsupported data type: %s", t))
      )
    })
    if (length(row[[1]]) == 0)
      break
    dat <- rbind(dat, as.data.frame(row))
  }
  if (all(c("SECONDS", "NANOSECONDS") %in% names(header)[1:2])) {
    dat <- cbind(
      with(dat,
           data.frame(TIMESTAMP = as.POSIXct(SECONDS + NANOSECONDS/1e9,
                                             origin="1990-01-01"))),
      dat[,-(1:2)])
    header <- cbind(data.frame(TIMESTAMP=c("TS", "", ""), header[, -(1:2)]))
  }
  new("csdf", data=dat, variables=header[-3,], meta=meta[,-1])
}
