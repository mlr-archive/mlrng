#' @include Measure.R
Measures$register(Measure$new(
  id = "mse",
  requires = "task.regr",
  fun = function(truth, predicted) mean((truth - predicted)^2)
))
