context("MlrModel")

test_that("MlrModel creation",{
  task = mlr.tasks$get("spam")
  lrn = mlr.learners$get("classif.dummy")
  mod = train(task, lrn)
  expect_is(mod, "MlrModel")
  expect_equal(mod$learner, lrn)
  expect_equal(mod$task, task)
  expect_equal(mod$train, task$active.rows)
  expect_is(mod$log, "TrainLog")
})
