#' @include Resamplings.R
Resamplings$add(
  Resampling$new(
    id = "subsampling",
    description = "subsampling",
    iters = 30L,
    instantiate = function(x) {
      if (inherits(x, "Task"))
        x = x$nrow
      self$set(train = replicate(self$iters, replace(logical(x), sample(x, floor(self$pars$ratio * x)), TRUE), simplify = FALSE))
    },
    pars = list(ratio = 2/3)
  )
)
