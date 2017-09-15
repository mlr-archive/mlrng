context("classif.dummy")

test_that("basic tests", {
  lrn = Learners$get("classif.dummy")
  expect_learner(lrn)
})
