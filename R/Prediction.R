Prediction = R6Class("Prediction",
  cloneable = TRUE,
  public = list(
    data = NULL,
    initialize = function(model, rowids, poutput) {
      assertR6(model, "MlrModel")
      task = model$task
      self$data = data.table(
        # FIXME: rowids seem to be integer here? should be character
        rowid = rowids,
        truth = asListOfRows(task$data(rows = rowids, cols = task$target)),
        poutput = asListOfRows(poutput)
      )
      invisible(self)
    }
  ),
  active = list(
    rowids = function() self$data$rowid
  )
)


PredictionClassif = R6Class("PredictionClassif",
  inherit = Prediction,
  active = list(
    response = function() as.character(self$data$poutput),
    truth = function() as.character(self$data$truth)
  )
)

PredictionRegr = R6Class("PredictionRegr",
  inherit = Prediction,
  active = list(
    response = function() as.numeric(self$data$poutput),
    truth = function() as.numeric(self$data$truth)
  )
)

#FIXME: really bad code, we need to create either depending on class of
# Task or WrappedModel
dispatchPrediction = function(task, model, rowids, poutput) {
  if (inherits(task, "TaskClassif"))
    PredictionClassif$new(model, rowids, poutput)
  else if (inherits(task, "TaskRegr"))
    PredictionRegr$new(model, rowids, poutput)
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
