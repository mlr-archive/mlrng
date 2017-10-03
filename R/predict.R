#' @export
predict.MlrModel = function(object, task, subset = NULL, ...) {
  assertR6(task, "Task")
  subset = translateSubset(task, subset)
  response = if (object$train.success)
    predictWorker(object$wrapped.model, task, object$learner, subset = subset)
  else
    predictFailureModel(object, task, subset = subset)
  dispatchPrediction(task, object, subset, response)
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
