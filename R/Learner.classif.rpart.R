#' @include Learner.R
Learners$add(Learner$new(
  type = "classif",
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
    as.character(predict(model, newdata = task[subset], type = "class", ...))
  }
))
