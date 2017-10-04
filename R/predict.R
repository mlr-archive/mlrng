#FIXME: predict REALLY has also to work with newdata!!!!

#' @export
predict.TrainResult = function(object, subset = NULL, ...) {
  task = object$task
  subset = translateSubset(task, subset)
  response = if (object$train.success)
    predictWorker(object$rmodel, task, object$learner, subset = subset)
  else
    predictFailureModel(object, task, subset = subset)
  dispatchPrediction(object, subset, response)
}

predictWorker = function(rmodel, task, learner, subset) {
  assertInteger(subset, lower = 1L, upper = task$nrow, any.missing = FALSE)

  pars = c(list(model = rmodel, task = task, subset = subset), learner$par.vals)
  do.call(learner$predict, pars)
}


predictFailureModel = function(model, task, subset) {
  fallback.learner = createFallbackLearner(task)
  predictWorker(model$rmodel, task, fallback.learner, subset)
}
