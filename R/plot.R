#' Plot csdf object
#'
#' A quick plotting routine to visualize TOA5 content. It requires reshape2, ggplot2, and gridExtra packages.
#'
#' @param x a \code{\link{csdf}} object for quick plotting
#' @param y ignored
#' @param ... ignored
#' @param ncol number of columns to arrange plots into
#' @param meta whether to include metadata from TOA5 header on a plot
#' @export
#'
#' @examples
#' Sys.setenv(TZ='GMT')
#' fpath <- system.file("extdata", "Station_Daily.dat", package="csdf")
#' obj <- read.toa5(fpath)
#' plot(obj)
setMethod("plot", signature(x="csdf", y="missing"), function(x, y, ..., ncol=2, meta=TRUE) {
  dummy <- sapply(c('reshape2', 'gridExtra', 'ggplot2'), function(package) {
    if (!requireNamespace(package))
      stop(sprintf('The package %s is not installed', x))
  })
  idx <- which(!grepl("_TM[xn]$|^RECORD$", names(x@data)))
  dat.long <- reshape2::melt(x@data[,idx], "TIMESTAMP")
  combined <- merge(dat.long,
                    data.frame(variable=names(x@variables),
                               units=unlist(x@variables[1,])))
  dd0 <- diff(x@data$TIMESTAMP)
  minstep <- min(dd0[dd0>0])
  dd <- diff(combined$TIMESTAMP)
  combined$grp <- cumsum(c(TRUE, dd != minstep))

  out <- by(data = combined, INDICES = droplevels(combined$units), FUN = function(m) {
    m <- droplevels(m)
    m <- ggplot2::ggplot(m, ggplot2::aes_string("TIMESTAMP", "value", group="grp", shape="variable", colour="variable")) +
      ggplot2::geom_point() + ggplot2::geom_line() +
      ggplot2::theme(legend.position="top",
                     axis.title.x=ggplot2::element_blank(),
                     axis.title.y=ggplot2::element_blank(),
                     legend.title=ggplot2::element_blank(),
                     panel.border=ggplot2::element_blank())
  })

  plots <- do.call(gridExtra::arrangeGrob, utils::modifyList(out, list(ncol=ncol)))
  if (meta) {
    headerGrob <- gridExtra::tableGrob(x@meta, rows=NULL)
    heights <- grid::unit.c(grid::unit(3, "lines"), grid::unit(1, "npc")-grid::unit(3, "lines"))
    gridExtra::grid.arrange(headerGrob, plots, ncol=1, heights=heights)
  } else
    grid::grid.draw(plots)
})
