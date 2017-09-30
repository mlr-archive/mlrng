#' @include Learners.R
Learners$add(Learner$new(
  type = "classif",
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
  predict.type = "response",
  properties = c("missings"),
  train = function(task, subset, data, ...) {
    rpart::rpart(task$formula, data)
  },
  predict = function(model, task, subset, data, ...) {
    pt = self$predict.type
    if (pt == "response")
      as.character(predict(model, newdata = data, type = "class", ...)) else
        predict(model, newdata = data, type = "prob", ...)
  }
))
