library(testthat)
library(checkmate)
library(stringi)

expect_task = function(task) {
  expect_is(task, "Task")
  expect_is(task$backend, "DataBackend")
  expect_string(task$id, min.chars = 1L)
  expect_class(task$formula, "formula")
  expect_list(task$hooks, names = "unique")
  expect_count(task$nrow)
  expect_count(task$ncol)
}

expect_supervisedtask = function(task) {
  expect_task(task)
  expect_is(task, "SupervisedTask")
  expect_choice(task$target, task$backend$cols)

  tf = terms(task$formula)
  expect_set_equal(labels(tf), task$features) # rhs
  expect_set_equal(setdiff(all.vars(tf), labels(tf)), task$target) # lhs
}

expect_classiftask = function(task) {
  expect_supervisedtask(task)
  expect_factor(task[[task$target]], any.missing = FALSE)
  cl = task$levels
  if (nlevels(cl) > 2L)
    expect_identical(task$positive, NA_character_)
  else
    expect_string(task$positive, na.ok = TRUE)
}

expect_regrtask = function(task) {
  expect_supervisedtask(task)
  expect_numeric(task[[task$target]], any.missing = FALSE)
}

expect_learner = function(lrn) {
  expect_is(lrn, "Learner")
  expect_list(lrn$hooks, names = "unique")
  expect_string(lrn$id, min.chars = 1L)
  expect_character(lrn$packages, min.chars = 1L)
  expect_is(lrn$par.set, "ParamSet")
  expect_list(lrn$par.vals, names = "unique")
  expect_function(lrn$predict, args = c("model", "task", "subset"), ordered = TRUE)
  expect_function(lrn$train, args = c("task", "subset"), ordered = TRUE)
  expect_subset(lrn$properties, choices = mlrng$learner.properties)
  expect_subset(lrn$type, choices = mlrng$learner.types)
}

expect_resampling = function(r, task) {
  expect_is(r, "Resampling")
  expect_identical(length(r), r$iters)
  expect_null(r$instance)

  r$reset()
  r$instantiate(task)
  n = task$nrow

  expect_list(r$instance, len = 2, names = "unique")
  expect_set_equal(names(r$instance), c("train", "test"))
  expect_true(all(BBmisc::vlapply(r$instance$train, bit::is.bit)))
  expect_true(all(BBmisc::vlapply(r$instance$test, bit::is.bit)))
  expect_identical(length(r$instance$train), r$iters)
  expect_identical(length(r$instance$test), r$iters)
  expect_equal(lengths(r$instance$train), rep(n, r$iters))
  expect_equal(lengths(r$instance$test), rep(n, r$iters))
  for (i in seq_len(r$iters)) {
    expect_integer(r$train(i), min.len = 1L, max.len = n - 1L, lower = 1L, upper = n, any.missing = FALSE, unique = TRUE, names = "unnamed")
    expect_integer(r$test(i), min.len = 1L, max.len = n - 1L, lower = 1L, upper = n, any.missing = FALSE, unique = TRUE, names = "unnamed")
  }
}

asDplyrTask = function(task) {
  task = getTask(task)

  requireNamespace("dplyr")
  con = dplyr::src_sqlite(":memory:", create = TRUE)
  tab = dplyr::copy_to(con, task$backend$data)

  newtask = task$clone(deep = TRUE)
  newtask$backend = DplyrBackend$new(tab, id.col = task$backend$id.col)
  newtask
}
