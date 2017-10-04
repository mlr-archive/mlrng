library(testthat)
library(checkmate)
library(stringi)

private = function(x) {
  if (!R6::is.R6(x))
    stop("Expected R6 class")
  x$.__enclos_env__[["private"]]
}


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
  expect_function(lrn$predict, args = c("model", "task", "subset"), ordered = TRUE)
  expect_function(lrn$train, args = c("task", "subset"), ordered = TRUE)
}

expect_split = function(s, len = NULL) {
  expect_class(s, "Split")
  expect_atomic_vector(s$train, min.len = 1)
  expect_atomic_vector(s$test, min.len = 1L)
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
      n = instantiated$nrow
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


# Dummy learner that can produce warnings/errors/messages
lrn.mock.regr = LearnerRegr$new(
  type = "regr",
  name = "mock",
  par.set = ParamHelpers::makeParamSet(
    ParamHelpers::makeDiscreteParam("method", values = c("mean", "median"), default = "mean"),
    ParamHelpers::makeLogicalParam("message", default = FALSE),
    ParamHelpers::makeLogicalParam("warning", default = FALSE),
    ParamHelpers::makeLogicalParam("error", default = FALSE)
  ),
  par.vals = list(),
  properties = c("missings", "factors", "numerics"),
  train = function(task, subset, method = "mean", message = FALSE, warning = FALSE, error = FALSE, ...) {
    tn = unlist(task$data(subset, task$target))
    mod = switch(method,
      "mean" = mean(tn),
      "median" = median(tn),
      stop("Illegal value for 'method'"))
    class(mod) = c("dummy.model", class(mod))
    if (message)
      message("dummy message")
    if (warning)
      warning("dummy warning")
    if (error)
      stop("dummy error")

    mod
  },

  predict = function(model, task, subset, ...) {
    as.numeric(model)
  }
)

