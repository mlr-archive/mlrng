#' @include mlr.resamplings.R
#' @include Resampling.R
mlr.resamplings$add(
  Resampling$new(
    id = "cv",
    iters = 10L,
    instantiate = function(task) {
      assertR6(task, "Task")
      n = task$nrow
      seq_len0 = function(n) seq(from = 0L, to = n - 1L)
      grp = sample.int(n) %% self$iters + 1L
      train.sets = lapply(seq_len(self$iters), function(i) which(i != grp))
      self$set(task, train.sets)
    }
  )
)
