context("classif.rpart")

test_that("basic tests", {
  lrn = Learners$get("classif.rpart")
  expect_learner(lrn)
})
