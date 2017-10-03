#' @include Dictionaries.R

mlr.learners$add(Learner$new(
  type = "regr",
  name = "rpart",
  package = "rpart",
  par.set = makeParamSet(
    makeIntegerParam("mtry", lower = 1)
  ),
  par.vals = list(),
  properties = c("missings"),
  train = function(task, subset, ...) {
    data = task$data(subset)
    rpart::rpart(task$formula, data)
  },
  predict = function(model, task, subset, ...) {
    data = task$data(subset, setdiff(task$active.cols, task$target))
    unname(predict(model, newdata = data, type = "vector", ...))
  }
))
