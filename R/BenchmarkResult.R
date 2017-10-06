#' @title BenchmarkResult
#' @format \code{\link{R6Class}} object
#'
#' @description
#' A \code{\link[R6]{R6Class}} containing data of a \code{\link{benchmark}}.
#'
#' @field data [\code{data.table}]: Data stored in a tabular format.
BenchmarkResult = R6Class("BenchmarkResult",
  inherit = ResampleResult,
  cloneable = FALSE,
  public = list(
    initialize = function(results, resampling.ids, resampling.iters) {
      assertAtomicVector(resampling.ids, len = length(results))
      assertAtomicVector(resampling.iters, len = length(results))
      self$data = rbindlist(lapply(results, function(x) x$data))
      self$data[, "resampling.id" := resampling.ids]
      self$data[, "resampling.iter" := resampling.iters][]
    }
  )
)
