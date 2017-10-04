PerforemanceResult = R6Class("PerforemanceResult",
  inherit = PredictResult,
  public = list(
    initialize = function(pred.result, perf.vals) {
      assertR6(pred.result, "PredictResult")
      self$dt = pred.result$dt
      self$dtgrow(perf.vals = perf.vals)
    }
  ),
  active = list(
    perf.vals = function() self$dt$perf.vals[[1L]]
  )
)

