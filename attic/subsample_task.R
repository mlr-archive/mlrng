    subsample = function(n = NULL, ratio = NULL) {
      if (is.null(n) + is.null(ratio) != 1L)
        stop("Either 'n' or 'ratio' must be not NULL")
      nr = self$nrow
      if (!is.null(n)) {
        self$active.rows = self$active.rows[sample(seq_len(nr), min(n, nr))]
      } else {
        self$active.rows = self$active.rows[sample(seq_len(nr), ratio * nr)]
      }
      invisible(self)
    }


  task$backend$subsample(ratio = 0.5)
  expect_identical(rtask$backend$nrow, 30L)

  task = Tasks$get("sonar")
  task$backend$subsample(ratio = 0.5)
  expect_identical(Tasks$get("sonar")$backend$nrow, 208L)
