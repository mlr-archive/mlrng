#' @title ResampleResult
#' @format [R6Class()] object
#'
#' @description
#' A [R6::R6Class()] containing data of a [resample()].
#'
#' @field aggr (`named numeric`): Aggregated performance measures.
#' @field data [`data.table`]: Data stored in a tabular format.
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
        gcat("[{i}/{iters}]: {stri_pasteNames(self$data$perf.vals[[i]])}")
      gcat("[Aggregated]: {self$aggr}")

      if (getOption("mlrng.debug", FALSE))
        cat("\n", format(self), "\n")
    }
  ),

  active = list(
    aggr = function() vnapply(rbindlist(lapply(self$data$perf.vals, as.list)), mean)
  )
)
