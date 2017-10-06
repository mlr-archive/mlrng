context("regr.ranger")

test_that("basic tests", {
  lrn = mlr.learners$get("regr.ranger")
  expect_learner(lrn)
})
