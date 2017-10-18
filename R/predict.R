#FIXME: predict REALLY has also to work with newdata!!!!

#' @export
predict.TrainResult = function(object, newdata = NULL, subset = NULL, ...) {
  if (...length() > 0L)
    stop("predict: dotargs currently unsupported!")
  task = object$task
  row.ids = translateSubset(task, subset)

  if (is.null(newdata)) {
    newdata = getTaskData(task, subset = row.ids, type = "test", props = object$learner$properties)
  } else {
    assertDataFrame(newdata)
    if (!is.null(subset))
      stop("Cannot use 'subset' together with 'newdata'")
  }

  predictWorker(object, newdata = newdata, row.ids)
}

predictWorker = function(train.result, newdata, row.ids) {
  learner = if (train.result$train.success) train.result$learner else createFallbackLearner(train.result$task)
  requireNS(learner$packages)
  pars = c(list(model = train.result, newdata = newdata), learner$par.vals)
  result = do.call(learner$predict, pars)
  PredictResult$new(train.result, row.ids, result)
}
