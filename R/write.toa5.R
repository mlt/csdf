#' Write TOA5 file from valid csdf class
#'
#' @param x \code{\link{csdf-class}} object
#' @param file a file to write
#' @param fileEncoding character string: if non-empty declares the encoding to
#'   be used on a file (not a connection) so the character data can be
#'   re-encoded as they are written. See \code{\link{file}}.
#'
#' @export
#'
# @examples
write.toa5 <- function(x, file="", fileEncoding="") {
  # taken from write.table
  if (file == "")
    file <- stdout()
  else if (is.character(file)) {
    file <- if (nzchar(fileEncoding))
      file(file, ifelse(append, "a", "w"), encoding = fileEncoding)
    else file(file, ifelse(append, "a", "w"))
    on.exit(close(file))
  }
  else if (!isOpen(file, "w")) {
    open(file, "w")
    on.exit(close(file))
  }
  if (!inherits(file, "connection"))
    stop("'file' must be a character string or connection")

  # Reformat POSIXct back into character to have time stamps enquoted
  idx <- which(grepl("_TM[xn]$|^TIMESTAMP$", names(x@data)))
  x@data[, idx] <- format(x@data[, idx], "%Y-%m-%d %H:%M:%S")
  write.table(c("TOA5", format(x@meta)), file, row.names = FALSE, col.names = FALSE, sep=",")
  write.csv(x@variables, file, row.names=FALSE)
  write.table(x@data, file, row.names=FALSE, col.names=FALSE, sep=",", na="\"NAN\"")
}
