Prediction = R6Class("Prediction",
  cloneable = FALSE,
  public = list(
    model = NULL,
    response = NULL,

    initialize = function(model, response) {
      self$task = assertR6(task, "Task")
      self$model = assertR6(wrapped.model, "WrappedModel")
      self$response = assertVector(response)
      data.table(
        model = list(model),

        truth = task$truth,
        response = list(response)
      )
    }
  )
)

#' @export
as.data.table.Prediction = function(x, keep.rownames = FALSE, ...) {
  data.table(
    task = x$task,
    model = x$wrapped.model,
    split = x$split,
    response = x$response
  )
}
