context("TaskClassif")

test_that("Basic ops on iris task", {
  dd = as.data.table(iris)
  task = Tasks$get("iris")
  expect_class(task, "TaskClassif")
  expect_equal(task$target, "Species")

  f = task$formula
  expect_class(f, "formula")
  expect_set_equal(attr(terms(f), "term.labels"), setdiff(names(dd), "Species"))
})
