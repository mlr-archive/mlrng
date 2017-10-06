#' @include Dictionaries.R

mlr.resamplings$add(
  Resampling$new(
    id = "subsampling",
    iters = 30L,
    instantiate = function(task) {
      assertR6(task, "Task")
      n = task$nrow
      self$set(task, train = replicate(self$iters, replace(logical(n), sample(n, floor(self$pars$ratio * n)), TRUE), simplify = FALSE))
    },
    pars = list(ratio = 2/3)
  )
)
