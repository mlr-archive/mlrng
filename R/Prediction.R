Prediction = R6Class("Prediction",
  cloneable = FALSE,
  public = list(
    task = NULL, # [Task]: ref to we trained on
    wrapped.model = NULL,
    split = NULL,
    response = NULL,

    initialize = function(task, wrapped.model, split, response) {
      self$task = assertR6(task, "Task")
      self$wrapped.model = assertR6(wrapped.model, "MlrModel")
      self$split = assertR6(split, "Split")
      self$response = assertVector(response)
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
