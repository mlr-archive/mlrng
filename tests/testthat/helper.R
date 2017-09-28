library(testthat)
library(checkmate)
library(stringi)

private = function(x) {
  if (!R6::is.R6(x))
    stop("Expected R6 class")
  x$.__enclos_env__[["private"]]
}

expect_task = function(task) {
  expect_is(task, "Task")
  expect_is(task$backend, "DataBackend")
  expect_string(task$id, min.chars = 1L)
  expect_class(task$formula, "formula")
  expect_count(task$backend$nrow)
  expect_count(task$backend$ncol)
  expect_subset(names(task$features), names(task$backend$get(task$backend$active.rows[1])))
}

expect_supervisedtask = function(task) {
  expect_task(task)
  expect_is(task, "TaskSupervised")
  expect_choice(task$target, task$backend$cols)

  tf = terms(task$formula)
  expect_set_equal(labels(tf), task$features) # rhs
  expect_set_equal(setdiff(all.vars(tf), labels(tf)), task$target) # lhs
}

expect_classiftask = function(task) {
  expect_supervisedtask(task)
  expect_factor(task$backend$get(cols = task$target)[[1L]], any.missing = FALSE)
  expect_int(task$nlevels, lower = 2L)
  if (task$nlevels > 2L)
    expect_identical(task$positive, NA_character_)
  else
    expect_string(task$positive, na.ok = TRUE)
}

expect_regrtask = function(task) {
  expect_supervisedtask(task)
  expect_numeric(task$backend$get(cols = task$target)[[1L]], any.missing = FALSE)
}

expect_learner = function(lrn) {
  expect_is(lrn, "Learner")
  expect_string(lrn$id, min.chars = 1L)
  expect_character(lrn$packages, min.chars = 1L)
  expect_is(lrn$par.set, "ParamSet")
  expect_list(lrn$par.vals, names = "unique")
  expect_function(lrn$predict, args = c("model", "task", "subset"), ordered = TRUE)
  expect_function(lrn$train, args = c("task", "subset"), ordered = TRUE)
  expect_subset(lrn$properties, choices = mlrng$learner.properties)
  expect_subset(lrn$type, choices = mlrng$learner.types)
}

expect_split = function(s, len = NULL) {
  expect_class(s, "Split")
  expect_bit(s$train.bit, len = len, min.1 = 1)
  expect_bit(s$test.bit, len = len, min.0 = 1)
  l = length(s$train.bit)
  expect_equal(l, length(s$test.bit))
  expect_integer(s$train, any.missing = FALSE, min.len = 1L, max.len = l, lower = 1L, upper = l)
  expect_integer(s$test, any.missing = FALSE, min.len = 1L, max.len = l, lower = 1L, upper = l)
}

# instantiated == NULL -> do not run tests for instance
# instantiated == FALSE -> assert that r is not instantiated
# instantiated == TRUE -> assert that r is instantiated
# instantiated == [task] -> assert that r is instantiated with task
expect_resampling = function(r, instantiated = NULL) {
  expect_is(r, "Resampling")
  expect_string(r$id, min.chars = 1L)
  expect_string(r$description, min.chars = 1L)
  expect_list(r$pars, names = "unique")
  expect_count(r$iters)

  if (isFALSE(instantiated)) {
    expect_scalar_na(r$checksum)
    expect_null(r$instance)
  }

  if (isTRUE(instantiated) || inherits(instantiated, "Task")) {
    expect_string(r$checksum)
    expect_list(r$instance, types = "Split", len = r$iters, names = "unnamed")

    if (inherits(instantiated, "Task")) {
      n = instantiated$backend$nrow
      for (i in seq_len(r$iters)) {
        expect_split(r$instance[[i]], len = n)
        expect_integer(r$train(i), min.len = 1L, max.len = n - 1L, lower = 1L, upper = n, any.missing = FALSE, unique = TRUE, names = "unnamed")
        expect_integer(r$test(i), min.len = 1L, max.len = n - 1L, lower = 1L, upper = n, any.missing = FALSE, unique = TRUE, names = "unnamed")
      }
    } else {
      for (i in seq_along(r)) expect_split(r$instance[[i]])
    }
  }
}

asDplyr = function(df) {
  requireNamespace("dplyr")
  con = dplyr::src_sqlite(":memory:", create = TRUE)
  dplyr::copy_to(con, df)
}

asDplyrTask = function(task) {
  if (testString(task))
    task = Tasks$get(task)

  requireNamespace("dplyr")
  con = dplyr::src_sqlite(":memory:", create = TRUE)
  tab = dplyr::copy_to(con, private(task$backend)$data)

  newtask = task$clone(deep = TRUE)
  newtask$backend = DataBackendDplyr$new(tab, id.col = task$backend$id.col)
  newtask
}
