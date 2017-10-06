context("classif.naiveBayes")

test_that("basic tests", {
  lrn = mlr.learners$get("classif.naiveBayes")
  expect_learner(lrn)
})
