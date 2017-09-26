#' @export
predict.WrappedModel = function(object, task, subset = NULL, ...) {
  assertR6(task, "Task")
  if (is.null(subset)) {
    split = object$split
  } else {
    subset = asSubset(task, subset)
    split = Split$new(train = object$split$train.bit, test = subset)
  }

  response = predictWorker(object$model, task, object$learner, split)
  Prediction$new(task, object, split, response)
}

predictWorker = function(model, task, learner, split) {
  i = split$test
  if (length(i) == 0L)
    stop("Cannot predict. Test set is empty.")
  cols = setdiff(task$backend$active.cols, task$target)
  pars = c(list(model = model, task = task, subset = i, data = task$backend$get(task$backend$active.rows[i], cols)), learner$par.vals)
  do.call(learner$predict, pars)
}
