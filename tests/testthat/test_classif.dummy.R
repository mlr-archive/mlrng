context("classif.dummy")

test_that("basic tests", {
  lrn = getLearner("classif.dummy")
  expect_learner(lrn)
})
