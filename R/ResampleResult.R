#' @title ResampleResult
#' @format \code{\link{R6Class}} object
#'
#' @description
#' A \code{\link[R6]{R6Class}} containing data of a \code{\link{resample}}.
#'
#' @field aggr [\code{named numeric}]: Aggregated performance measures.
#' @field data [\code{data.table}]: Data stored in a tabular format.
ResampleResult = R6Class("ResampleResult",
  cloneable = FALSE,
  public = list(
    data = NULL,
    measure = NULL,
    initialize = function(result, measure) {
      self$data = result
      self$measure = measure
    }
  ),
  active = list(
    aggr = function() {
      mean(unlist(lapply(self$data, "[[", "performance")))
    }
  )
)

#' @export
as.data.table.ResampleResult = function(x, keep.rownames = FALSE, ...) {
  rbindlist(lapply(x$data, function(x) {
    c(list(
      predicted = list(x$predicted),
      model = list(x$model)
    ), unlist(x$performance))
  }), idcol = "iter")
}
