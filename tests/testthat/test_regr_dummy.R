context("regr.dummy")

test_that("basic tests", {
  lrn = getLearner("regr.dummy")
  expect_learner(lrn)
})
