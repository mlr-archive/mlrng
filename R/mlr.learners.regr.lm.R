#' @include Dictionaries.R

mlr.learners$add(LearnerRegr$new(
  name = "lm",
  package = "stats",
  par.set = makeParamSet(
    makeNumericLearnerParam(id = "tol", default = 1e-7, lower = 0),
    makeLogicalLearnerParam(id = "singular.ok", default = TRUE, tunable = FALSE)
  ),
  par.vals = list(),
  properties = c("feat.numeric", "feat.factor", "se", "weights"),
  train = function(task, subset, weights = NULL, ...) {
    data = getTaskData(task, subset = subset, type = "train", props = self$properties)
    if (is.null(weights))
      lm(task$formula, data, ...)
    else
      lm(task$formula, data, weights = weights,...)
  },
  predict = function(model, newdata, ...) {
    if (self$predict.type == "response") {
      predict(model$rmodel, newdata = newdata, se.fit = FALSE, ...)
    } else {
      p = predict(model$rmodel, newdata = newdata, se.fit = TRUE, ...)
      cbind(p$fit, p$se.fit)
    }
  }
))
