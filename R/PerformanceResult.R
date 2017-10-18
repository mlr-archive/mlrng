#' @include PredictResult.R
PerformanceResult = R6Class("PerformanceResult",
  inherit = PredictResult,
  cloneable = FALSE,
  public = list(
    initialize = function(pred.result, measures, perf.vals) {
      assertR6(pred.result, "PredictResult")
      assertList(measures, "Measure")
      self$data = pred.result$data
      self$data[, "measures" := list(measures)]
      self$data[, "perf.vals" := list(list(assertNumeric(perf.vals, min.len = 1L)))]
    },

    print = function(...) {
      gcat("[Performance]: task={self$task$id} | learner={self$learner$id} | {stri_peek(stri_pasteNames(self$perf.vals, sep = '='))}")
      if (getOption("mlrng.debug", FALSE))
        cat("\n", format(self), "\n")
    }
  ),
  active = list(
    perf.vals = function() self$data$perf.vals[[1L]]
  )
)
