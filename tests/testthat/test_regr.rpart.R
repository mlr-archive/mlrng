context("regr.rpart")

test_that("basic tests", {
  lrn = Learners$get("regr.rpart")
  expect_learner(lrn)
})
