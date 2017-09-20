#' @include Resamplings.R
Resamplings$add(
  Resampling$new(
    id = "holdout",
    description = "holdout",
    iters = 1L,
    instantiate = function(x) {
      if (inherits(x, "Task"))
        x = x$backend$nrow
      self$set(train = list(replace(logical(x), sample(x, floor(self$pars$ratio * x)), TRUE)))
    },
    pars = list(ratio = 2/3)
  )
)
