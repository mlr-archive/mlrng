context("TaskRegr")

test_that("Basic ops on BostonHousing task", {
  task = test.tasks$get("regr.num")
  expect_regrtask(task)
  expect_equal(task$target, "medv")

  f = task$formula
  expect_class(f, "formula")
  expect_set_equal(attr(terms(f), "term.labels"), c("b", "lstat", "ptratio"))

  task$summary
})
