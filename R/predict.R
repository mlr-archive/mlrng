#' @export
predict.MlrModel = function(object, task, subset = NULL, ...) {
  assertR6(task, "Task")
  subset = translateSubset(task, subset)
  split = Split$new(train = object$train, test = subset)

  response = if (object$train.success)
    predictWorker(object$model, task, object$learner, subset = split$test)
  else
    predictFailureModel(object$model, task, split$test)

  Prediction$new(task, object, split, response)
}

predictWorker = function(model, task, learner, subset) {
  assertInteger(subset, lower = 1L, upper = task$nrow, any.missing = FALSE)

  pars = c(list(model = model, task = task, subset = subset), learner$par.vals)
  do.call(learner$predict, pars)
}


predictFailureModel = function(model, task, subset) {
  fallback.learner = createFallbackLearner(task)
  predictWorker(model, task, fallback.learner, subset)
}
