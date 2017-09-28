#' @export
predict.WrappedModel = function(object, task, subset = NULL, ...) {
  assertR6(task, "Task")
  if (is.null(subset)) {
    split = object$split
  } else {
    subset = asSubset(task, subset)
    split = Split$new(train = object$split$train.bit, test = subset)
  }

  response = predictWorker(object$model, task, object$learner, subset = split$test)
  Prediction$new(task, object, split, response)
}

predictWorker = function(model, task, learner, subset) {
  assertInteger(subset, lower = 1L, upper = task$backend$nrow, any.missing = FALSE)

  pars = c(list(model = model, task = task, subset = subset, data = task$backend$get(task$backend$active.rows[subset], task$features)), learner$par.vals)
  do.call(learner$predict, pars)
}
