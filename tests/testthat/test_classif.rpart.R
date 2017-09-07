context("classif.rpart")

test_that("basic tests", {
  lrn = getLearner("classif.rpart")
  expect_learner(lrn)
})
