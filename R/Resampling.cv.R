#' @include Resampling.R
Resamplings$register(
  Resampling$new(
    id = "cv",
    description = "cross-validation",
    instantiate = function(x) {
      if (inherits(x, "Task"))
        x = x$nrow
      assertCount(x)
      seq_len0 = function(n) seq(from = 0L, to = n - 1L)
      self$instance = outer(X = sample(seq_len0(x) %% self$pars$iters), Y = seq_len0(self$pars$iters), "!=")
    },
    pars = list(iters = 10L)
  )
)
