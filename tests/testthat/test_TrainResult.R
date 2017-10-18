context("TrainResult")

test_that("TrainResult is valid",{
  task = test.tasks$get("clm.num")
  lrn = mlr.learners$get("classif.dummy")
  tr = train(task, lrn)

  expect_r6(tr, c("TrainResult", "Result"), ordered = TRUE)
  expect_trainresult(tr)
  expect_equal(tr$learner, lrn)
  expect_equal(tr$task, task)
  expect_true(tr$train.success)
})
