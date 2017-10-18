#' @include TrainResult.R
PredictResult = R6Class("PredictResult",
  inherit = TrainResult,
  cloneable = FALSE,
  public = list(
    initialize = function(train.result, test.set, predicted) {
      assertR6(train.result, "TrainResult")
      assertAtomicVector(test.set)
      self$data = train.result$data
      self$data[, c("test.set", "predicted") := list(list(test.set), list(predicted))]
    },

    print = function(...) {
      gcat("Prediction result of {self$learner$id} trained on {self$task$id}.")
      if (getOption("mlrng.debug", FALSE))
        cat("\n", format(self), "\n")
    }
  ),

  active = list(
    test.set = function() self$data$test.set[[1L]],
    truth = function() self$data$task[[1L]]$truth(rows = self$data$test.set[[1L]]),
    predicted = function() self$data$predicted[[1L]],
    pred = function() {
      data.table(
        test.set = self$data$test.set[[1L]],
        truth = self$truth[[1]],
        response = self$data$predicted[[1L]]
      )
    }
  )
)
