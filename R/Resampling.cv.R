#' @include Resamplings.R
Resamplings$add(
  Resampling$new(
    id = "cv",
    description = "cross-validation",
    iters = 10L,
    instantiate = function(x) {
      if (inherits(x, "Task"))
        x = x$backend$nrow
      assertCount(x)
      seq_len0 = function(n) seq(from = 0L, to = n - 1L)
      m = outer(X = sample(seq_len0(x) %% self$iters), Y = seq_len0(self$iters), "!=")
      self$set(train = lapply(seq_col(m), function(i) m[, i]))
    }
  )
)
