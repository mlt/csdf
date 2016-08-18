#' Read unsigned long from connection as numeric
#'
#' @param con a \code{\link{connection}} object. It cannot be a character string naming a
#'   file or a raw vector as it reads by words and will start over for the second word.
#' @param endian The endian-ness ("big" or "little") of the target system for
#'   the file. Files produced with LoggerNet on MS Windows are little-endian.
#'
#' @return numeric
read.ulong <- function(con, endian) {
  low <- readBin(con, integer(), size=2, signed=FALSE, endian=endian)
  high <- readBin(con, integer(), size=2, signed=FALSE, endian=endian)
  if(endian=="little")
    as.numeric(high) * 2^16 + low
  else
    as.numeric(low) * 2^16 + high
}
