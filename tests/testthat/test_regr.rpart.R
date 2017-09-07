context("regr.rpart")

test_that("basic tests", {
  lrn = getLearner("regr.rpart")
  expect_learner(lrn)
})
