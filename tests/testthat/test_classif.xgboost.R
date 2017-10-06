context("classif.xgboost")

test_that("basic tests", {
  lrn = mlr.learners$get("classif.xgboost")
  expect_learner(lrn)
})
