#' @include Dictionaries.R

mlr.measures$add(Measure$new(
  id = "mmce",
  description = "Mean misclassification error",
  task.types = c("classif"),
  fun = function(truth, predicted) mean(truth != predicted)
))

mlr.measures$add(Measure$new(
  id = "mse",
  description = "Mean squared error",
  task.types = c("regr"),
  fun = function(truth, predicted) mean((truth - predicted)^2)
))

