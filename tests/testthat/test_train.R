context("train")

test_that("train", {
  task = Tasks$get("spam")
  lrn = Learners$get("classif.dummy")
  mod = train(task, lrn)
  expect_is(mod, "WrappedModel")
  p = predict(mod, task, subset = seq_len(task$backend$nrow))
  expect_subset(p$predicted, task$levels)
})

test_that("train on dplyr task", {
  task = asDplyrTask(Tasks$get("spam"))
  lrn = Learners$get("classif.dummy")
  mod = train(task, lrn)
  expect_is(mod, "WrappedModel")
  p = predict(mod, task, subset = seq_len(task$backend$nrow))
  expect_subset(p$predicted, task$levels)
})
