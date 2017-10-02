context("train")

test_that("train", {
  task = mlr.tasks$get("spam")
  lrn = mlr.learners$get("classif.dummy")
  mod = train(task, lrn)
  expect_is(mod, "WrappedModel")
  p = predict(mod, task, subset = seq_len(task$nrow))
  expect_subset(p$predicted, task$levels(task$target))
})
