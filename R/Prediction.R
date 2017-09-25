Prediction = R6Class("Prediction",
  cloneable = FALSE,
  public = list(
    task = NULL, # [Task]: ref to we trained on
    data = NULL, # [data.table]: prediction cols, (id, response, truth, ...)

    initialize = function(task, subset, response) {
      self$task = task
      ids = task$backend$active.rows[subset]
      self$data = data.table(
        id = ids,
        response = response,
        truth = task$backend$get(ids = ids, cols = task$target)
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
