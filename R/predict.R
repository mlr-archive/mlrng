#FIXME: predict REALLY has also to work with newdata!!!!

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
  response = predictWorker(object,
    learner = learner,
    newdata = newdata %??% getTaskData(task, subset = subset, type = "test", props = learner$properties)
  )
  PredictResult$new(object, subset, response)
}

predictWorker = function(train.result, learner, newdata) {
  requireNS(learner$packages)
  pars = c(list(model = train.result, newdata = newdata), learner$par.vals)
  do.call(learner$predict, pars)
}
