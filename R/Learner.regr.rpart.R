#' @include Learners.R
Learners$add(Learner$new(
  type = "regr",
  name = "rpart",
  package = "rpart",
  par.set = makeParamSet(
    makeIntegerParam("mtry", lower = 1)
  ),
  par.vals = list(),
  properties = c("missings"),
  train = function(task, subset, data, ...) {
    rpart::rpart(task$formula, data)
  },
  predict = function(model, task, subset, data, ...) {
    unname(predict(model, newdata = data, type = "vector", ...))
  }
))
