Result = R6Class("Result",
  cloneable = FALSE,
  public = list(
    data = NULL,
    initialize = function(data) {
      self$data = data
    }
  )
)
