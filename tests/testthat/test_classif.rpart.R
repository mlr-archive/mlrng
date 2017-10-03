context("classif.rpart")

test_that("basic tests", {
  lrn = mlr.learners$get("classif.rpart")
  expect_learner(lrn)
})
