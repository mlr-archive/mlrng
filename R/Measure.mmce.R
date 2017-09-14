#' @include Measure.R
Measures$register(Measure$new(
  id = "mmce",
  name = "Mean misclassification error",
  tasktypes = c("classif"),
  requires = c("pred", "pred.truth", "pred.response"),
  fun = function(truth, predicted) mean(truth != predicted)
))
