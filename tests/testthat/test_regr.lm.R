context("regr.lm")

test_that("basic tests", {
  lrn = mlr.learners$get("regr.lm")
  expect_learner(lrn)
})
