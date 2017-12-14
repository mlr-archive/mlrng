#' @title ResampleResult
#' @format \code{\link{R6Class}} object
#'
#' @description
#' A \code{\link[R6]{R6Class}} containing data of a \code{\link{resample}}.
#'
#' @field aggr [\code{named numeric}]: Aggregated performance measures.
#' @field data [\code{data.table}]: Data stored in a tabular format.
ResampleResult = R6Class(c("ResampleResult", "PerformanceResult", "PredictResult", "TrainResult"),
  inherit = Result,
  cloneable = FALSE,
  public = list(
    initialize = function(results, resampling.iters = seq_along(results)) {
      assertList(results, types = "PerformanceResult")
      self$data = rbindlist(lapply(results, function(x) x$data))
      self$data[, "resampling.iter" := resampling.iters][]
    },

    print = function(...) {
      iters = nrow(self$data)
      gcat("[Resample result]: task={self$data$task[[1]]$id} | learner={self$data$learner[[1]]$id}")
      for (i in seq_len(iters))
        gcat("[{i}/{iters}]: {stri_paste_names(self$data$perf.vals[[i]])}")
      gcat("[Aggregated]: {self$aggr}")

      if (getOption("mlrng.debug", FALSE))
        cat("\n", format(self), "\n")
    }
  ),

  active = list(
    aggr = function() vnapply(rbindlist(lapply(self$data$perf.vals, as.list)), mean)
  )
)
