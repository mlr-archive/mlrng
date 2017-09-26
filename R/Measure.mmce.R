#' @include Measures.R
Measures$add(Measure$new(
  id = "mmce",
  description = "Mean misclassification error",
  tasktypes = c("classif"),
  fun = function(truth, predicted) mean(truth != predicted)
))
