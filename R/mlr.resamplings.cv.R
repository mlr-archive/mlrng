#' @include Dictionaries.R

mlr.resamplings$add(
  Resampling$new(
    id = "cv",
    description = "cross-validation",
    iters = 10L,
    instantiate = function(task) {
      assertR6(task, "Task")
      n = task$nrow
      seq_len0 = function(n) seq(from = 0L, to = n - 1L)
      m = outer(X = sample(seq_len0(n) %% self$iters), Y = seq_len0(self$iters), "!=")
      train = lapply(seq_col(m), function(i) m[, i])
      self$set(task, train)
    }
  )
)
