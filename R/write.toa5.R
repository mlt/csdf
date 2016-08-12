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
  out <- within(x@data, {
    for (n in names(x@data)[idx])
      assign(n, format(get(n), "%Y-%m-%d %H:%M:%S"))
    rm(n)
  })

  # make sure we aren't messing with column order
  stopifnot(names(x@data) == names(out))

  write.table(c("TOA5", format(x@meta)), file, row.names = FALSE, col.names = FALSE, sep=",")
  suppressWarnings( write.csv(x@variables, file, TRUE, row.names=FALSE) )
  write.table(out, file, TRUE, row.names=FALSE, col.names=FALSE, sep=",", na="\"NAN\"")
}
