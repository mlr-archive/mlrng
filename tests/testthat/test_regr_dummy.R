context("regr.dummy")

test_that("basic tests", {
  lrn = Learners$get("regr.dummy")
  expect_learner(lrn)
})
