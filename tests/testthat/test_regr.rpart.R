context("regr.rpart")

test_that("basic tests", {
  lrn = mlr.learners$get("regr.rpart")
  expect_learner(lrn)
})
