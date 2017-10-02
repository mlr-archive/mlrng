context("MlrModel")

test_that("MlrModel creation",{
  task = Tasks$get("spam")
  lrn = Learners$get("classif.dummy")
  mod = train(task, lrn)
  expect_is(mod, "MlrModel")
  expect_equal(mod$learner, lrn)
  expect_equal(mod$task, task)
  expect_equal(mod$train, task$active.rows)
  expect_is(mod$log, "TrainLog")
})
