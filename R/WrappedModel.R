WrappedModel = R6Class("WrappedModel",
  cloneable = FALSE,
  public = list(
    model = NULL,
    learner = NULL,
    initialize = function(task, learner, model) {
      self$learner = learner
      self$model = model
    }
  )
)
