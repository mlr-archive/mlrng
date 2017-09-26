Prediction = R6Class("Prediction",
  cloneable = FALSE,
  public = list(
    task = NULL, # [Task]: ref to we trained on
    data = NULL, # [data.table]: prediction cols, (id, response, truth, ...)
    wrapped.model = NULL,
    split = NULL,

    initialize = function(task, wrapped.model, split, response) {
      self$task = assertR6(task, "Task")
      self$wrapped.model = assertR6(wrapped.model, "WrappedModel")
      self$split = assertR6(split, "Split")
      ids = task$backend$active.rows[split$test]
      self$data = data.table(
        id = ids,
        response = response,
        truth = task$backend$get(ids = ids, cols = task$target)[[1L]]
      )
    }
  ),

  active = list(
    # [vec]. get response vec
    response = function() self$data[["response"]],
    # [vec]. get truth vec
    truth = function() self$data[["truth"]],
    # [vec]. get id vec
    ids = function() self$data[["id"]]
  )
)

#' @export
as.data.table.Prediction = function(x, keep.rownames = FALSE, ...) {
  return(x$data)
}
