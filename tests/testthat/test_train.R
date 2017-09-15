context("train")

test_that("train", {
  task = Tasks$get("spam")
  lrn = Learners$get("classif.dummy")
  mod = train(task, lrn)
  expect_is(mod, "WrappedModel")
})

test_that("train on dplyr task", {
  task = asDplyrTask("spam")
  lrn = Learners$get("classif.dummy")
  mod = train(task, lrn)
  expect_is(mod, "WrappedModel")
  p = predict(mod, task)
  expect_subset(p$predicted, task$levels)
})
