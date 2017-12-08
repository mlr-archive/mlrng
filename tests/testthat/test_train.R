context("train")

task = test.tasks$get("clm.num")
lrn = mlr.learners$get("classif.rpart")

test_that("Training with default options", {
  mod = train(task, lrn)
  expect_is(mod, "TrainResult")
  expect_true(mod$train.success)
  expect_result(mod)
  p = predict(mod)
  expect_is(p, "PredictResult")
  expect_subset(p$predicted, task$classes)
  expect_result(mod)
})

test_that("No training encapsulation works", {
  withr::with_options(new = list(mlrng.train.encapsulation = "none"), code = {
    mod = train(task, lrn)
    expect_is(mod, "TrainResult")
    expect_true(mod$train.success)
    p = predict(mod)
    expect_subset(p$predicted, task$classes)
  })
})

#FIXME: We can't test this, since we have to call library(mlrng) which does not work if the package is not installed.
#test_that("Training in external R session works", {
#  withr::with_options(new = list(mlrng.train.encapsulation = 2), code = {
#    mod = train(task, lrn)
#    expect_is(mod, "TrainResult")
#    expect_true(mod$train.success)
#    p = predict(mod, task)
#    expect_subset(p$predicted, task$backend$distinct(task$target))
#  })
#})

test_that("warnings/messages are caught", {
  task = test.tasks$get("regr.num")
  lrn = test.learner$get("regr.mock")
  lrn$par.vals = list(warning = TRUE, message = TRUE)
  mod = train(task, lrn)
  expect_equal(mod$train.log$n.messages, 1)
  expect_equal(mod$train.log$n.warnings, 1)
  expect_equal(mod$train.log$messages[[1]]$message, "dummy message\n")
  expect_equal(mod$train.log$warnings[[1]]$message, "dummy warning")
})

test_that("continue on learner error works", {
  withr::with_options(new = list(mlrng.continue.on.learner.error = TRUE), code = {
    lrn = test.learner$get("regr.mock")
    lrn$par.vals = list(error = TRUE)
    task = test.tasks$get("regr.num")
    mod = train(task, lrn)
    expect_false(mod$train.success)
    expect_equal(mod$train.log$n.errors, 1)
    expect_equal(mod$train.log$errors[[1]]$message, "dummy error")
    p = predict(mod, subset = seq_len(task$nrow))
    expect_numeric(p$predicted, len = task$nrow)
  })
})
