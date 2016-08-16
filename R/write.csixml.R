#' Write csdf object to a file in CSI XML format
#'
#' We write a text file directly without XML object to avoid memory overhead for
#' large datasets.
#'
#' @param x a \code{\linkS4class{csdf}} object
#' @param file a file to write to
#'
#' @export
#'
#' @examples
#' Sys.setenv(TZ='GMT')
#' fpath <- system.file("extdata", "Station_Daily.dat", package="csdf")
#' obj <- read.toa5(fpath)
#' write.csixml(obj, "out.xml")
write.csixml <- function(x, file="") {
  # the following is taken from write.table
  if (file == "")
    file <- stdout()
  else if (is.character(file)) {
    file <- file(file, "w", encoding = "UTF-8")
    on.exit(close(file))
  }
  else if (!isOpen(file, "w")) {
    open(file, "w")
    on.exit(close(file))
  }
  if (!inherits(file, "connection"))
    stop("'file' must be a character string or connection")

  cat(with(x@meta, sprintf('<?xml version="1.0" standalone="yes"?>
<csixml version="1.0">
  <head>
    <environment>
      <station-name>%s</station-name>
      <table-name>%s</table-name>
      <model>%s</model>
      <serial-no>%s</serial-no>
      <os-version>%s</os-version>
      <dld-name>%s</dld-name>
      <dld-sig>%s</dld-sig>
    </environment>
    <fields>\n', station, table, model, serial, os, dld, signature)), file=file)
  n <- names(x@variables)
  bare <- !(n %in% c("TIMESTAMP", "RECORD"))
  lapply(n[bare], function(v) {
    cat(sprintf('      <field name="%s" process="%s" type="%s" units="%s" />\n',
                v, x@variables['process', v],
                switch(as.character(x@variables['process', v]),
                       TMn='xsd:dateTime',
                       TMx='xsd:dateTime',
                       'xsd:float'
                ), x@variables['units', v]), file=file)
  })
  cat('    </fields>
  </head>
  <data>\n', file=file)

  # Reformat POSIXct back into character to have time stamps enquoted
  idx <- which(grepl("_TM[xn]$|^TIMESTAMP$", names(x@data)))
  x@data[, idx] <- format(x@data[, idx], "%Y-%m-%dT%H:%M:%S")

  # apply causes unnecessary formatting during coercion
  # apply(x@data, 1, function(r) {
  for(i in 1:nrow(x@data)) {
    no <- ifelse(is.na(x@data[i, 'RECORD']), i-1, x@data[i, 'RECORD'])
    ts <- ifelse(is.na(x@data[i, 'TIMESTAMP']), "", sprintf('time="%s"', x@data[i, 'TIMESTAMP']))
    row <- as.character(x@data[i, bare])
    row <- ifelse(row == "NA", "&quot;NAN&quot;", row)
    left <- sprintf("<v%d>", 1:length(row))
    right <- sprintf("</v%d>", 1:length(row))
    cat(sprintf('<r no="%d" %s>', no, ts),
        paste(c(rbind(left, row, right)), collapse=""),
        "</r>\n", file=file, sep="")
  }

  cat('  </data>
</csixml>\n', file=file)
}
