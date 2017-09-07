#' @include Measure.R
Measures$register(Measure$new(
  id = "mmce",
  requires = "task.classif",
  fun = function(truth, predicted) mean(truth != predicted)
))
