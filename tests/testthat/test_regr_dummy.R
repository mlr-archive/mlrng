context("regr.dummy")

test_that("basic tests", {
  lrn = mlr.learners$get("regr.dummy")
  expect_learner(lrn)
})
