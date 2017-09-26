#' @title BenchmarkResult
#' @format \code{\link{R6Class}} object
#'
#' @description
#' A \code{\link[R6]{R6Class}} containing data of a \code{\link{benchmark}}.
#'
#' @field data [\code{data.table}]: Data stored in a tabular format.
BenchmarkResult = R6Class("BenchmarkResult",
  cloneable = FALSE,
  public = list(
    data = NULL,
    initialize = function() {
      self$data = data.table(
        task = character(0L),
        learner = character(0L),
        resampling.id = character(0L),
        resampling.iter = integer(0L),
        model = list(),
        response = list()
      )
    },

    add = function(results) {
      self$data = rbind(self$data, results, fill = TRUE)
      invisible(self)
    }
  )
)

#' @export
as.data.table.BenchmarkResult = function(x, keep.rownames = FALSE, ...) {
  copy(x$data)
}
