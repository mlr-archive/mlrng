#' @include Measures.R
Measures$add(Measure$new(
  id = "mse",
  name = "Mean squared error",
  tasktypes = c("regr"),
  requires = c("pred", "pred.truth", "pred.response"),
  fun = function(truth, predicted) mean((truth - predicted)^2)
))
