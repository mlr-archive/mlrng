#FIXME: predict REALLY has also to work with newdata!!!!

#FIXME: we IGNORE ...? due to s3 inheritance, but check this

#' @export
predict.TrainResult = function(object, newdata = NULL, subset = NULL, ...) {
  if (...length() > 0L)
    stop("predict: dotargs currently unsupported!")
  if (!is.null(newdata) && !is.null(subset))
    stop("Use 'subset' only without 'newdata'")
  task = object$task
  assertIndexSet(subset, for.task = task)
  subset = translateSubset(task, subset)
  learner = if (object$train.success) object$learner else createFallbackLearner(task)
  if (is.null(newdata))
    newdata = getTaskData(task, subset = subset, type = "test", props = learner$properties)
  response = learner$predict(model = object, newdata = newdata)
  PredictResult$new(object, subset, response)
}
