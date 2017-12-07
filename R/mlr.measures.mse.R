#' @include mlr.measures.R
#' @include Measure.R
mlr.measures$add(Measure$new(
  id = "mse",
  description = "Mean squared error",
  task.types = c("regr"),
  fun = function(pred) {
    mean((pred$truth[[1L]] - pred$predicted)^2)
  }
))
