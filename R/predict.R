#FIXME: predict REALLY has also to work with newdata!!!!

#' @export
predict.TrainResult = function(object, newdata = NULL, subset = NULL, ...) {
  if (...length() > 0L)
    stop("predict: dotargs currently unsupported!")
  if (!is.null(newdata) && !is.null(subset))
    stop("Use 'subset' only without 'newdata'")
  task = object$task
  row.ids = translateSubset(task, subset)

  predictWorker(object,
    newdata = newdata %??% getTaskData(task, subset = row.ids, type = "test", props = object$learner$properties),
    row.ids
  )
}

predictWorker = function(train.result, newdata, row.ids) {
  requireNS(learner$packages)
  learner = if (train.result$train.success) train.result$learner else createFallbackLearner(task)
  pars = c(list(model = train.result, newdata = newdata), learner$par.vals)
  result = do.call(learner$predict, pars)
  PredictResult$new(train.result, row.ids, result)
}
