PredictResult = R6Class("PredictResult",
  inherit = TrainResult,
  public = list(
    initialize = function(train.result, test.set, poutput) {
      assertR6(train.result, "TrainResult")
      # FIXME: rowids seem to be integer here? should be character
      #FIXME: what hapens with the ref here? do we clone? DANGEROUS!
      self$dt = train.result$dt
      self$dtgrow(test.set = test.set, poutput = poutput)
    }
  ),
  active = list(
    test.set = function() self$dt$test.set[[1L]],
    poutput = function() self$dt$poutput[[1L]],
    response = function() self$dt$poutput[[1L]],
    truth = function() self$dt$task$truth[[1L]]
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
