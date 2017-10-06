#FIXME: predict REALLY has also to work with newdata!!!!

#FIXME: we IGNORE ...? due to s3 inheritance, but check this

#' @export
predict.TrainResult = function(object, task = object$task, subset = NULL, newdata = NULL, ...) {
  if (length(list(...)) > 0L)
    stop("predict: dotargs currently unsupported!")
  learner = object$learner
  if (is.null(newdata))
    newdata = getTaskData(task, subset = subset, type = "test", props = learner$properties)
  assertIndexSet(subset, for.task = task)
  subset = translateSubset(task, subset)
  response = if (object$train.success)
    predictWorker(object, learner, newdata)
  else
    predictFailureModel(object, newdata)
  dispatchPrediction(object, subset, response)
}

predictWorker = function(model, learner, newdata) {
  learner$predict(model = model, newdata = newdata)
}

predictFailureModel = function(model, newdata) {
  fallback.learner = createFallbackLearner(task)
  predictWorker(model, fallback.learner, newdata)
}
