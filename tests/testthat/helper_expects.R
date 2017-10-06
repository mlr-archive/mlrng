expect_view = function(v, data = NULL, rowid = NULL) {
  expect_r6(v, "View",
    cloneable = TRUE,
    public = c("active.cols", "active.rows", "con", "tbl", "raw.tbl", "name", "pars", "rowid.col"),
    private = c("view.cols", "view.rows"))
  expect_character(v$active.cols, any.missing = FALSE, unique = TRUE)
  if (!is.null(data))
    expect_subset(v$active.cols, names(data))
  expect_atomic_vector(v$active.rows, any.missing = FALSE, unique = TRUE)
  if (!is.null(data) && !is.null(rowid))
    expect_subset(v$active.rows, data[[rowid]])
  expect_true(DBI::dbIsValid(v$con))
  expect_class(v$tbl, c("tbl_sql", "tbl_lazy", "tbl"))
  expect_class(v$raw.tbl, c("tbl_sql", "tbl_lazy", "tbl"))
  expect_string(v$name)
  expect_string(v$checksum)
  expect_list(v$pars, names = "unique")
  expect_string(v$rowid.col)
  expect_integer(v$nrow, len = 1L, lower = 0L, upper = nrow(data) %??% Inf)
  expect_integer(v$ncol, len = 1L, lower = 0L, upper = nrow(data) %??% Inf)
  expect_character(v$types, names = "unique")
  expect_subset(v$types, mlrng$supported.col.types)
  if (!is.null(data))
    expect_subset(names(v$types), names(data))
  expect_tibble(dplyr::collect(head(v$tbl, 1L)), nrow = 1L)
  expect_tibble(dplyr::collect(head(v$raw.tbl, 1L)), nrow = 1L)
}

expect_task = function(task) {
  expect_r6(task, "Task")
  expect_string(task$id, min.chars = 1L)
  expect_count(task$nrow)
  expect_count(task$ncol)
  expect_view(task$view)
  expect_data_table(task$data(task$view$active.rows[1]))
  expect_data_table(task$head(1))
  task.nas = task$na.cols
  expect_integer(task.nas, names = "unique", any.missing = FALSE, lower = 0L, upper = task$nrow)
  expect_set_equal(names(task.nas), task$view$active.cols)
}

expect_supervisedtask = function(task) {
  expect_task(task)
  expect_is(task, "TaskSupervised")
  expect_choice(task$target, task$view$active.cols)

  expect_class(task$formula, "formula")
  tf = terms(task$formula)
  expect_set_equal(labels(tf), task$features) # rhs
  expect_set_equal(setdiff(all.vars(tf), labels(tf)), task$target) # lhs
  expect_subset(names(task$features), colnames(task$view$tbl))
}

expect_classiftask = function(task) {
  expect_supervisedtask(task)
  expect_factor(task$data(cols = task$target)[[1L]], any.missing = FALSE)
  expect_int(task$nlevels, lower = 2L)
  if (task$nlevels > 2L)
    expect_identical(task$positive, NA_character_)
  else
    expect_string(task$positive, na.ok = TRUE)
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

# task == NULL -> do not run tests for instance
# task == FALSE -> assert that r is not instantiated
# task == TRUE -> assert that r is instantiated
# task == [task] -> assert that r is instantiated with task
expect_resampling = function(r, task = NULL) {
  expect_is(r, "Resampling")
  expect_string(r$id, min.chars = 1L)
  expect_list(r$pars, names = "unique")
  expect_count(r$iters)

  if (isFALSE(task)) {
    expect_scalar_na(r$checksum)
    expect_null(r$instance)
  }

  if (isTRUE(task) || inherits(task, "Task")) {
    expect_string(r$checksum)
    expect_list(r$instance, types = "Split", len = r$iters, names = "unnamed")

    if (inherits(task, "Task")) {
      n = task$nrow
      for (i in seq_len(r$iters)) {
        expect_split(r$instance[[i]], len = n)
        expect_integer(r$train.set(task, i), min.len = 1L, max.len = n - 1L, lower = 1L, upper = n, any.missing = FALSE, unique = TRUE, names = "unnamed")
        expect_integer(r$test.set(task, i), min.len = 1L, max.len = n - 1L, lower = 1L, upper = n, any.missing = FALSE, unique = TRUE, names = "unnamed")
      }
    } else {
      for (i in seq_len(r$iters)) expect_split(r$instance[[i]])
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

