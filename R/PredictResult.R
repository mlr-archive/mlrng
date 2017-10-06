PredictResult = R6Class("PredictResult",
  inherit = TrainResult,
  public = list(
    initialize = function(train.result, test.set, poutput) {
      assertR6(train.result, "TrainResult")
      self$dt = train.result$dt
      self$dtgrow(
        test.set = assertIndexSet(test.set, for.task = train.result$task),
        #FIXME: which assertions should go here? learner group defines this
        poutput = poutput
      )
    },
    print = function(...) {
      gcat("Prediction result of {self$learner$id} trained on {self$task$id}.")
      if (getOption("mlrng.debug", TRUE))
        cat("\n", format(self), "\n")
    }
  ),
  active = list(
    test.set = function() self$dt$test.set[[1L]],
    poutput = function() self$dt$poutput[[1L]],
    response = function() self$dt$poutput[[1L]],
    truth = function() self$dt$task[[1L]]$truth(rows = self$test.set),
    pred = function() {
      data.table(
        test.set = self$dt$test.set[[1L]],
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
