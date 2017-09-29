#' @export
predict.WrappedModel = function(object, task, subset = NULL, ...) {
  assertR6(task, "Task")
  subset = asSubset(task, subset)
  split = Split$new(train = object$train, test = subset)

  response = predictWorker(object$model, task, object$learner, subset = split$test)
  Prediction$new(task, object, split, response)
}

predictWorker = function(model, task, learner, subset) {
  assertInteger(subset, lower = 1L, upper = task$backend$nrow, any.missing = FALSE)

  pars = c(list(model = model, task = task, subset = subset, data = task$backend$get(i = subset, cols = task$features)), learner$par.vals)
  do.call(learner$predict, pars)
}
