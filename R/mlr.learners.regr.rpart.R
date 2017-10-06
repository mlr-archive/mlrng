#' @include Dictionaries.R

mlr.learners$add(LearnerRegr$new(
  name = "rpart",
  package = "rpart",
  par.set = makeParamSet(
    makeIntegerLearnerParam(id = "minsplit", default = 20L, lower = 1L),
    makeNumericLearnerParam(id = "cp", default = 0.01, lower = 0, upper = 1),
    makeIntegerLearnerParam(id = "maxcompete", default = 4L, lower = 0L),
    makeIntegerLearnerParam(id = "maxsurrogate", default = 5L, lower = 0L),
    makeDiscreteLearnerParam(id = "usesurrogate", default = 2L, values = 0:2),
    makeDiscreteLearnerParam(id = "surrogatestyle", default = 0L, values = 0:1),
    # we use 30 as upper limit, see docs of rpart.control
    makeIntegerLearnerParam(id = "maxdepth", default = 30L, lower = 1L, upper = 30L),
    makeIntegerLearnerParam(id = "xval", default = 10L, lower = 0L, tunable = FALSE)
  ),
  par.vals = list(),
  properties = c("missings", "feat.numeric", "feat.factor", "feat.ordered", "oobpreds", "featimp", "se", "formula"), # FIXME: Does rpart really support se estimation?
  train = function(task, subset, ...) {
    data = getTaskData(task, subset = subset, type = "train", props = self$properties)
    rpart::rpart(task$formula, data, ...)
  },
  predict = function(model, newdata, ...) {
    unname(predict(model$rmodel, newdata = newdata, type = "vector", ...))
  }
))
