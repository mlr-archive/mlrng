#' @include Dictionaries.R

mlr.learners$add(LearnerClassif$new(
  name = "naiveBayes",
  package = "e1071",
  par.set = makeParamSet(
    makeNumericLearnerParam(id = "laplace", default = 0, lower = 0)
  ),
  par.vals = list(),
  properties = c("twoclass", "multiclass", "missings", "feat.numeric", "feat.factor", "prob"),
  train = function(task, subset, ...) {
    data = getTaskData(task, subset = subset, type = "train", props = self$properties)
    e1071::naiveBayes(task$formula, data = data, ...)
  },
  predict = function(model, newdata, ...) {
    type = ifelse(self$predict.type == "response", "class", "raw")
    unname(predict(model$rmodel, newdata = newdata, type = type, ...))
  }
))
