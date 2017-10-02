context("classif.dummy")

test_that("basic tests", {
  lrn = mlr.learners$get("classif.dummy")
  expect_learner(lrn)
})
