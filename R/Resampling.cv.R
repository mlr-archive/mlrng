#' @include Resampling.R
Resamplings$register(
  Resampling$new(
    id = "cv",
    description = "cross-validation",
    iters = 10L,
    instantiate = function(x) {
      if (inherits(x, "Task"))
        x = x$nrow
      assertCount(x)
      seq_len0 = function(n) seq(from = 0L, to = x - 1L)
      self$instance = outer(X = sample(seq_len0(x) %% self$iters), Y = seq_len0(self$iters), "!=")
    }
  )
)
