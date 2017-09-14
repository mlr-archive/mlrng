#' @include Resampling.R
Resamplings$register(
  Resampling$new(
    id = "subsampling",
    description = "subsampling",
    iters = 30L,
    instantiate = function(x) {
      if (inherits(x, "Task"))
        x = x$nrow
      private$setInstance(train = replicate(self$iters, replace(logical(x), sample(x, floor(self$pars$ratio * x)), TRUE), simplify = FALSE))
    },
    pars = list(ratio = 2/3)
  )
)
