#' @include TrainResult.R
PredictResult = R6Class("PredictResult",
  inherit = TrainResult,
  public = list(
    initialize = function(train.result, test.set, poutput) {
      assertR6(train.result, "TrainResult")
      assertAtomicVector(test.set)
      self$data = train.result$data
      self$data[, c("test.set", "poutput") := list(list(test.set), list(poutput))]
    },

    print = function(...) {
      gcat("Prediction result of {self$learner$id} trained on {self$task$id}.")
      if (getOption("mlrng.debug", TRUE))
        cat("\n", format(self), "\n")
    }
  ),

  active = list(
    test.set = function() self$data$test.set[[1L]],
    poutput = function() self$data$poutput[[1L]],
    response = function() self$data$poutput[[1L]],
    truth = function() self$data$task[[1L]]$truth(rows = self$test.set),
    pred = function() {
      data.table(
        test.set = self$data$test.set[[1L]],
        truth = self$truth[[1]],
        response = self$response
      )
    }
  )
)
