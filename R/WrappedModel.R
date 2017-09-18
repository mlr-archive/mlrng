WrappedModel = R6Class("WrappedModel",
  cloneable = FALSE,
  public = list(
    task = NULL,
    model = NULL,
    learner = NULL,
    resampling = NULL,
    initialize = function(task, learner, model, resampling) {
      self$task = task
      self$learner = learner
      self$model = model
      self$resampling = resampling
    }
  )
)
