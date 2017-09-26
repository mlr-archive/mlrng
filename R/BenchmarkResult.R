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
    initialize = function(tasks, learners, resamplings) {
      fun = function(resampling) {
        CJ(task = names(tasks), learner = names(learners), resampling = resampling$id, iter = seq_len(resampling$iters))
      }
      self$data = rbindlist(lapply(resamplings, fun))
    },

    store = function(res) {
      self$data = cbind(self$data, rbindlist(lapply(res, function(x) {
        c(list(
          split = list(x$split),
          model = list(x$model),
          predicted = list(x$predicted)),
        unlist(x$performance))
      })))
    }
  )
)

#' @export
as.data.table.BenchmarkResult = function(x, keep.rownames = FALSE, ...) {
  copy(x$data)
}
