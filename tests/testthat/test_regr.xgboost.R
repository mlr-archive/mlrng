context("regr.xgboost")

test_that("basic tests", {
  lrn = mlr.learners$get("regr.xgboost")
  expect_learner(lrn)
})
