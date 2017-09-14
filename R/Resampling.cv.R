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
      seq_len0 = function(n) seq(from = 0L, to = n - 1L)
      m = outer(X = sample(seq_len0(x) %% self$iters), Y = seq_len0(self$iters), "!=")
      private$setInstance(train = lapply(seq_col(m), function(i) m[, i]))
    }
  )
)
