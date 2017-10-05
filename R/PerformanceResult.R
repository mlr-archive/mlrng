PerforemanceResult = R6Class("PerforemanceResult",
  inherit = PredictResult,
  public = list(
    initialize = function(pred.result, perf.vals) {
      assertR6(pred.result, "PredictResult")
      self$dt = pred.result$dt
      self$dtgrow(
        perf.vals = assertNumeric(perf.vals, min.len = 1L)
      )
    },
    print = function(...)
      gcat("[Performance]: task={self$task$id} | learner={self$learner$id} | {stri_peek(stri_pasteNames(self$perf.vals, sep = '='))}")
  ),
  active = list(
    perf.vals = function() self$dt$perf.vals[[1L]]
  )
)

