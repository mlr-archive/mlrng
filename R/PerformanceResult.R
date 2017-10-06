#' @include PredictResult.R
PerformanceResult = R6Class("PerformanceResult",
  inherit = PredictResult,
  public = list(
    initialize = function(pred.result, perf.vals) {
      assertR6(pred.result, "PredictResult")
      self$data = pred.result$data
      self$data[, "perf.vals" := list(list(assertNumeric(perf.vals, min.len = 1L)))]
    },
    print = function(...)
      gcat("[Performance]: task={self$task$id} | learner={self$learner$id} | {stri_peek(stri_pasteNames(self$perf.vals, sep = '='))}")
  ),
  active = list(
    perf.vals = function() self$data$perf.vals[[1L]]
  )
)
