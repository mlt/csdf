#' Write TOA5 file from valid csdf class
#'
#' @param x \code{\link{csdf-class}} object
#' @param file a file to write
#'
#' @export
#'
# @examples
write.toa5 <- function(x, file="") {
  # Reformat POSIXct back into character to have time stamps enquoted
  idx <- which(grepl("_TM[xn]$|^TIMESTAMP$", names(x@data)))
  x@data[, idx] <- format(x@data[, idx], "%Y-%m-%d %H:%M:%S")
  write.table(c("TOA5", format(x@meta)), file, row.names = FALSE, col.names = FALSE, sep=",")
  suppressWarnings( write.csv(x@variables, file, TRUE, row.names=FALSE) )
  write.table(x@data, file, TRUE, row.names=FALSE, col.names=FALSE, sep=",", na="\"NAN\"")
}
