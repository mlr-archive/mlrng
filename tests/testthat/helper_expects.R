expect_backend = function(b) {
  expect_r6(b, cloneable = TRUE, public = c("nrow", "ncol", "colnames", "rownames", "data", "head", "distinct", "missing.values", "types"))
  n = b$nrow
  p = b$ncol
  expect_count(n)
  expect_count(p)
  expect_atomic_vector(b$rownames, any.missing = FALSE, len = n)
  expect_character(b$colnames, any.missing = FALSE, len = p, min.chars = 1L, unique = TRUE)
  expect_data_table(b$data, nrow = n, ncol = p, col.names = "unique")

  cn = b$colnames[1L]
  x = b$get(cols = cn)
  expect_data_table(x, ncol = 1, nrow = n)
  x = x[[cn]]
  expect_atomic_vector(x, len = n)
  expect_set_equal(b$distinct(cn), x)

  types = b$types
  expect_character(types, len = p, names = "unique")
  expect_set_equal(names(types), b$colnames)
  expect_subset(types, mlrng$supported.col.types)

  mv = b$missing.values
  expect_integer(mv, names = "unique", any.missing = FALSE, lower = 0, upper = n)
  expect_set_equal(names(mv), b$colnames)

  expect_data_table(b$head(3), nrow = 3, ncol = p)
}


expect_task = function(task) {
  expect_r6(task, "Task", cloneable = TRUE)
  expect_string(task$id, min.chars = 1L)
  expect_count(task$nrow)
  expect_count(task$ncol)
  expect_backend(task$backend)
  expect_data_table(task$data)
  expect_data_table(task$get())
  expect_data_table(task$head(1), nrow = 1L)
  # task.nas = task$na.cols
  # expect_integer(task.nas, names = "unique", any.missing = FALSE, lower = 0L, upper = task$nrow)
  # expect_set_equal(names(task.nas), task$backend$colnames)
}

expect_supervisedtask = function(task) {
  expect_task(task)
  expect_is(task, "TaskSupervised")
  expect_choice(task$target, task$backend$colnames)

  expect_class(task$formula, "formula")
  tf = terms(task$formula)
  expect_set_equal(labels(tf), task$features) # rhs
  expect_set_equal(setdiff(all.vars(tf), labels(tf)), task$target) # lhs
  expect_subset(names(task$features), colnames(task$backend$tbl))
}

expect_classiftask = function(task) {
  expect_supervisedtask(task)
  x = task$truth()[[1L]]
  expect_atomic_vector(x, any.missing = FALSE)
  expect_true(is.character(x) || is.factor(r))
  expect_int(task$nclasses, lower = 2L)
  expect_atomic_vector(task$classes)
  expect_subset(task$classes, x)
  if (task$nclasses > 2L)
    expect_identical(task$positive, NA_character_)
  else
    expect_choice(task$positive, task$classes)
}

expect_regrtask = function(task) {
  expect_supervisedtask(task)
  expect_numeric(task$get(cols = task$target)[[1L]], any.missing = FALSE)
}

expect_learner = function(lrn) {
  expect_is(lrn, "Learner")
  expect_string(lrn$id, min.chars = 1L)
  expect_character(lrn$packages, min.chars = 1L)
  expect_subset(lrn$properties, mlrng$supported.learner.props)
  expect_is(lrn$par.set, "ParamSet")
  expect_list(lrn$par.vals, names = "unique")
  expect_function(lrn$predict, args = c("model", "newdata"), ordered = TRUE)
  expect_function(lrn$train, args = c("task", "subset"), ordered = TRUE)
}

expect_split = function(s, len = NULL) {
  expect_class(s, "Split")
  expect_atomic_vector(s$train.set, min.len = 1)
  expect_atomic_vector(s$test.set, min.len = 1L)
}

# task == FALSE -> assert that r is not instantiated
# task == [task] -> assert that r is instantiated with task
expect_resampling = function(r, task = FALSE) {
  expect_is(r, "Resampling")
  expect_string(r$id, min.chars = 1L)
  expect_list(r$pars, names = "unique")
  expect_count(r$iters)

  if (isFALSE(task)) {
    expect_scalar_na(r$checksum)
    expect_null(r$instance)
  }

  if (inherits(task, "Task")) {
    expect_string(r$checksum)
    expect_list(r$instance, len = 2)
    expect_list(r$instance$train, len = r$iters, names = "unnamed")
    expect_list(r$instance$test, len = r$iters, names = "unnamed")

    n = task$nrow
    rows = task$backend$rownames
    for (i in seq_len(r$iters)) {
      expect_atomic_vector(r$train.set(i), min.len = 1L, max.len = n - 1L, any.missing = FALSE, names = "unnamed")
      expect_subset(r$train.set(i), rows)
      expect_atomic_vector(r$test.set(i), min.len = 1L, max.len = n - 1L, any.missing = FALSE, names = "unnamed")
      expect_subset(r$test.set(i), rows)
    }
  }
}

expect_result = function(x) {
  expect_r6(x, "Result", public = "data")
  cols = list(
    TrainResult = c("task", "learner", "rmodel", "train.set", "train.log"),
    PredictResult = c("test.set", "poutput"),
    PerformanceResult = c("perf.vals"),
    ResampleResult = c("resampling.iter"),
    BenchmarkResult = c("resampling.id")
  )
  i = max(match(class(x), names(cols), nomatch = 0L))
  cols = unlist(head(cols, i), use.names = FALSE)

  if (!is.null(x$print))
    expect_output(print(x))
  expect_data_table(x$data, min.rows = 1L)
  expect_subset(cols, names(x$data))
}

expect_same_address = function(x, y) {
  expect_identical(address(x), address(y))
}

expect_different_address = function(x, y) {
  expect_false(identical(address(x), address(y)))
}
