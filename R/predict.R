#FIXME: predict REALLY has also to work with newdata!!!!

#FIXME: we IGNORE ...? due to s3 inheritance, but check this

#' @export
predict.TrainResult = function(object, subset = NULL, ...) {
  if (length(list(...)) > 0L)
    stop("predict: dotargs currently unsupported!")
  task = object$task
  assertIndexSet(subset, for.task = task)
  subset = translateSubset(task, subset)
  response = if (object$train.success)
    predictWorker(object$rmodel, task, object$learner, subset = subset)
  else
    predictFailureModel(object, task, subset = subset)
  dispatchPrediction(object, subset, response)
}

predictWorker = function(rmodel, task, learner, subset) {
  pars = c(list(model = rmodel, task = task, subset = subset), learner$par.vals)
  do.call(learner$predict, pars)
}


predictFailureModel = function(model, task, subset) {
  fallback.learner = createFallbackLearner(task)
  predictWorker(model$rmodel, task, fallback.learner, subset)
}
