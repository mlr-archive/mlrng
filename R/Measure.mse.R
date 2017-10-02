#' @include Measures.R
Measures$add(Measure$new(
  id = "mse",
  description = "Mean squared error",
  task.types = c("regr"),
  fun = function(truth, predicted) mean((truth - predicted)^2)
))
