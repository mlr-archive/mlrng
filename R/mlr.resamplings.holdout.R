#' @include mlr.resamplings.R
#' @include Resampling.R
mlr.resamplings$add(
  Resampling$new(
    id = "holdout",
    iters = 1L,
    instantiate = function(task) {
      assertR6(task, "Task")
      n = task$nrow
      train.set = sample.int(n, floor(self$pars$ratio * n))
      self$set(task = task, train.sets = list(train.set))
    },
    pars = list(ratio = 2/3)
  )
)
