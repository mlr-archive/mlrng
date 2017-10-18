context("ResampleResult")

test_that("ResampleResult is valid", {
  task = test.tasks$get("clm.num")
  learner = mlr.learners$get("classif.dummy")
  resampling = mlr.resamplings$get("cv")
  resampling$iters = 3
  measures = list(mlr.measures$get("mmce"))

  rr = resample(task = task, learner = learner, resampling = resampling, measures = measures)
  expect_trainresult(rr)
  expect_predictresult(rr)

  expect_resampleresult(rr)
})

test_that("RR from runExperiment", {
  task = test.tasks$get("clm.num")
  learner = mlr.learners$get("classif.rpart")
  resampling = mlr.resamplings$get("cv")
  resampling$instantiate(task)
  measures = list(mlr.measures$get("mmce"))
  rs = lapply(1:2, runExperiment, task = task, learner = learner, resampling = resampling, measures = measures)
  rr = ResampleResult$new(rs)
  expect_resampleresult(rr)
  expect_data_table(rr$data, nrow = 2)
})
