#' @include TrainResult.R
PredictResult = R6Class("PredictResult",
  inherit = TrainResult,
  public = list(
    initialize = function(train.result, test.set, poutput) {
      assertR6(train.result, "TrainResult")
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


PredictResultClassif = R6Class("PredictResultClassif",
  inherit = PredictResult
)

PredictResultRegr = R6Class("PredictResultRegr",
  inherit = PredictResult
)

#FIXME: really bad code, we need to create either depending on class of
# Task or WrappedModel
dispatchPrediction = function(train.result, test.set, poutput) {
  task = train.result$task
  if (inherits(task, "TaskClassif"))
    PredictResultClassif$new(train.result, test.set, poutput)
  else if (inherits(task, "TaskRegr"))
    PredictResultRegr$new(train.result, test.set, poutput)
  else
    stop("bam")
}


#' @export
# as.data.table.Prediction = function(x, keep.rownames = FALSE, ...) {
#   data.table(
#     task = x$task,
#     model = x$wrapped.model,
#     split = x$split,
#     response = x$response
#   )
# }
