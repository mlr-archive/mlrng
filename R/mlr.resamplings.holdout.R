#' @include Dictionaries.R

mlr.resamplings$add(
  Resampling$new(
    id = "holdout",
    description = "holdout",
    iters = 1L,
    instantiate = function(task) {
      assertR6(task, "Task")
      n = task$nrow
      train = replace(logical(n), sample(n, floor(self$pars$ratio * n)), TRUE)
      self$set(task = task, train = list(train))
    },
    pars = list(ratio = 2/3)
  )
)
