#' @include mlr.measures.R
#' @include Measure.R
mlr.measures$add(Measure$new(
  id = "mmce",
  description = "Mean misclassification error",
  task.types = c("classif"),
  fun = function(pred) {
    mean(pred$truth[[1L]] != pred$predicted)
  }
))
