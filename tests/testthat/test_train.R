context("train")

test_that("train", {
  task = mlr.tasks$get("spam")
  lrn = mlr.learners$get("classif.dummy")
  mod = train(task, lrn)
  expect_is(mod, "MlrModel")
  expect_true(mod$train.success)
  p = predict(mod, task)
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

test_that("continue on learner error works", {
  withr::with_options(new = list(mlrng.continue.on.learner.error = TRUE), code = {
    task = mlr.tasks$get("bh")
    lrn.mock.regr$par.vals = list(error = TRUE)
    mod = train(task, lrn.mock.regr)
    expect_false(mod$train.success)
    expect_equal(mod$log$n.errors, 1)
    expect_equal(mod$log$errors[[1]]$message, "dummy error")
    p = predict(mod, task, subset = seq_len(task$nrow))
    expect_numeric(p$response, len = task$nrow)
  })
})
