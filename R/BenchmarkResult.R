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
    initialize = function(experiments) {
      self$data = rbindlist(experiments)
    }
  ),
  active = list(
    flat = function() {
      res = data.table(
        task.id = ids(x$task),
        learner.id = ids(x$learner),
        resampling.id = ids(x$resampling),
        chksum = vcapply(x$resampling, "[[", "checksum")
        )
      cbind(res, rbindlist(x$performance))
    },
    aggr = function() {
      self$flat[, list(mmce = mean(mmce)), by = list(task.id, learner.id, chksum)][, "!chksum"]
    }
  )
)
