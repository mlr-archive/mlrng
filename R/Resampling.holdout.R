#' @include Resampling.R
Resamplings$register(
  Resampling$new(
    id = "holdout",
    description = "holdout",
    iters = 1L,
    instantiate = function(x) {
      if (inherits(x, "Task"))
        x = x$nrow
      private$setInstance(train = list(as.bit(replace(logical(x), sample(x, floor(self$pars$ratio * x)), TRUE))))
    },
    pars = list(ratio = 2/3)
  )
)
