#' @include Dictionaries.R


mlr.measures$add(Measure$new(
  id = "mmce",
  description = "Mean misclassification error",
  task.types = c("classif"),
  fun = function(pred) mean(pred$truth != pred$response)
))

mlr.measures$add(Measure$new(
  id = "mse",
  description = "Mean squared error",
  task.types = c("regr"),
  fun = function(pred) mean((pred$truth - pred$response)^2)
))

