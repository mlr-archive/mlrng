#' @include Resampling.R
Resamplings$register(
  Resampling$new(
    id = "subsampling",
    description = "subsampling",
    instantiate = function(x) {
      if (inherits(x, "Task"))
        x = x$nrow
      self$instance = replicate(self$pars$iters, replace(logical(x), sample(x, floor(self$pars$ratio * x)), TRUE), simplify = TRUE)
    },
    pars = list(iters = 30L, ratio = 2/3)
  )
)
