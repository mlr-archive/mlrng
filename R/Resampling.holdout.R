#' @include Resampling.R
Resamplings$register(
  Resampling$new(
    id = "holdout",
    description = "holdout",
    iters = 1L,
    instantiate = function(x) {
      if (inherits(x, "Task"))
        x = x$nrow
      self$instance = matrix(replace(logical(x), sample(x, floor(self$pars$ratio * x)), TRUE), ncol = 1L)
    },
    pars = list(ratio = 2/3)
  )
)
