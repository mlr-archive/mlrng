#' @include Measure.R
mlr.measures$add(Measure$new(
  id = "mmce",
  description = "Mean misclassification error",
  task.types = c("classif"),
  fun = function(pred) {
    mean(pred$truth[[1L]] != pred$predicted)
  }
))

mlr.measures$add(Measure$new(
  id = "mse",
  description = "Mean squared error",
  task.types = c("regr"),
  fun = function(pred) {
    mean((pred$truth[[1L]] - pred$predicted)^2)
  }
))

