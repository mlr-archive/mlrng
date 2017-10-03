context("train")

test_that("train", {
  task = mlr.tasks$get("spam")
  lrn = mlr.learners$get("classif.dummy")
  mod = train(task, lrn)
  expect_is(mod, "MlrModel")
  p = predict(mod, task, subset = seq_len(task$nrow))
  expect_subset(p$predicted, task$levels(task$target))
})


test_that("warnings/messages are caught", {
  task = mlr.tasks$get("bh")
  lrn.mock.regr$par.vals = list(warning = TRUE, message = TRUE)
  mod = train(task, lrn.mock.regr)
  expect_equal(mod$log$n.messages, 1)
  expect_equal(mod$log$n.warnings, 1)
  expect_equal(mod$log$messages[[1]]$message, "dummy message\n")
  expect_equal(mod$log$warnings[[1]]$message, "dummy warning")
})
