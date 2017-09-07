#' @include Learner.R
Learners$register(Learner$new(
  type = "regr",
  name = "rpart",
  package = "rpart",
  par.set = makeParamSet(
    makeIntegerParam("mtry", lower = 1)
  ),
  par.vals = list(),
  properties = c("missings"),
  train = function(task, subset, ...) {
    rpart::rpart(task$formula, task[subset])
  },
  predict = function(model, task, subset, ...) {
    unname(predict(model, newdata = task[subset], type = "vector", ...))
  }
))
