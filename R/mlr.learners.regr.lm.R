#' @include Dictionaries.R

mlr.learners$add(LearnerRegr$new(
  type = "regr",
  name = "lm",
  package = "stats",
  par.set = makeParamSet(
    makeNumericLearnerParam(id = "tol", default = 1e-7, lower = 0),
    makeLogicalLearnerParam(id = "singular.ok", default = TRUE, tunable = FALSE)
  ),
  par.vals = list(),
  properties = c("numerics", "factors", "se", "weights"),
  train = function(task, subset, weights = NULL, ...) {
    data = getTaskData(task, subset, type = "train")
    if (is.null(weights))
      lm(task$formula, data, ...)
    else
      lm(task$formula, data, weights = weights,...)
  },
  predict = function(model, task, subset, ...) {
    data = getTaskData(task, subset, type = "test")
    if (self$predict.type == "response") {
      unnamed(predict(model, newdata = data, se.fit = FALSE, ...))
    } else {
      p = predict(model, newdata = data, se.fit = TRUE, ...)
      cbind(unname(p$fit), unname(p$se.fit))
    }
  }
))
