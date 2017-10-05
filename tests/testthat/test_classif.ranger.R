context("classif.ranger")

test_that("basic tests", {
  lrn = mlr.learners$get("classif.ranger")
  expect_learner(lrn)
})
