#' Get common information about stored data
#'
#' @param object \code{\linkS4class{csdf}} object to get summary for
#'
#' @return a \code{\link{list}} summarizing stored data with \code{summary.csdf}
#'   \code{\link{class}} assigned to trigger \code{\link{print.summary.csdf}} S3
#'   method.
#'
#' List components are:
#' \describe{
#' \item{\emph{granularity}}{\code{\link{difftime}} for smallest positive
#' non-zero interval between measurements.}
#' \item{\emph{ts.ok}}{\code{TRUE} if TIMESTAMP column in @data
#' slot is monotonic. If \code{FALSE}, potentially unintentional clock
#' correction might have taken place, or other serious data problem.}
#' \item{\emph{rn.ok}}{\code{TRUE} if RECORD is sequential in slot data of csdf object.
#' CR program reload results in RECORD counter reset.
#' If TOA5 file contains restarts it may indicate that
#' parts of a file were produced with a different CR Basic program.}
#' }
#' \item{\emph{nogap}}{\code{TRUE} if adjacent records are \emph{granularity}
#' apart. You might have issues if \code{FALSE}.}
#' \item{\emph{gaps}}{a \code{\link{vector} containing TIMESTAMP befor gaps.}}
#' \item{\emph{from}}{initial/minimal TIMESTAMP}
#' \item{\emph{to}}{final/maximal TIMESTAMP}
#'
#' @export
setMethod("summary", "csdf", function(object) {
  dd0 <- diff(object@data$TIMESTAMP)
  minstep <- min(dd0[dd0>0])
  z <- list(
    rn.ok=with(object@data, all(diff(object@data$RECORD)==1L) && object@data[1, 'RECORD'] == 0L),
    ts.ok=with(object@data, all(TIMESTAMP == cummax(as.numeric(TIMESTAMP)))),
    granularity=minstep,
    nogap=all(dd0==minstep),
    gaps=object@data$TIMESTAMP[which(dd0!=minstep)],
    from=min(object@data$TIMESTAMP),
    to=max(object@data$TIMESTAMP)
  )
  class(z) <- 'summary.csdf'
  z
})


#' Print common information about stored data
#'
#' @param x an object to print summary for
#' @param ... ignored
#'
#' @export
print.summary.csdf <- function(x, ...) {
  with(x, {
    cat('Data coverage is from ',
        format(from), ' to ', format(to),
        ' with temporal granularity of ',
        granularity, ' ', attr(granularity, "units"),
        '. There are', if (nogap) ' no' else '', ' gaps in data coverage.\n', sep="")
    if (!nogap)
      cat("Gaps are at: ", paste(gaps, collapse=", "), "\n", sep="")
    cat('There are', if (ts.ok) ' no' else '', ' issues with TIMESTAMP.\n', sep="")
    if(!is.na(rn.ok))
      cat('There are', if (rn.ok) ' no' else '', ' issues with RECORD.\n', sep="")
    else
      cat('There is no RECORD counter.\n')
  })
}
