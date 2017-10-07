#' @include Dictionaries.R

mlr.resamplings$add(
  Resampling$new(
    id = "subsampling",
    iters = 30L,
    instantiate = function(task) {
      assertR6(task, "Task")
      n = task$nrow

      train.sets = replicate(self$iters, sample.int(n, floor(self$pars$ratio * n)), simplify = FALSE)
      self$set(task, train.sets)
    },
    pars = list(ratio = 2/3)
  )
)
