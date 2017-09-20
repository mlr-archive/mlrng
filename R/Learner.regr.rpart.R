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
  train = function(task, subset, ...) {
    rpart::rpart(task$formula, task$backend$get(subset))
  },
  predict = function(model, task, subset, ...) {
    unname(predict(model, newdata = task$backend$get(subset), type = "vector", ...))
  }
))
