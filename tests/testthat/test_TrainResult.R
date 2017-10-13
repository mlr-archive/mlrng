context("TrainResult")

test_that("TrainResult creation",{
  task = mlr.tasks$get("spam")
  lrn = mlr.learners$get("classif.dummy")
  mod = train(task, lrn)
  expect_is(mod, "TrainResult")
  expect_equal(mod$learner, lrn)
  expect_equal(mod$task, task)
  expect_equal(mod$train.set, task$backend$rownames)
  expect_is(mod$train.log, "TrainLog")
})
