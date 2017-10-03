context("resample")

test_that("runExperiment worker", {
  task = mlr.tasks$get("iris")
  learner = mlr.learners$get("classif.rpart")
  resampling = mlr.resamplings$get("cv")
  resampling$instantiate(task)
  resampling.iter = 1L
  measures = list(mlr.measures$get("mmce"))

  res = list()
  for (i in 1:10) {
    res[[i]] = runExperiment(task, learner, resampling, i, measures)
  }
  rr = rbindlist(res)
  res = resample(task, learner, resampling, measures)
  expect_is(res, "ResampleResult")
})

test_that("Basic resampling", {
  task = mlr.tasks$get("iris")
  learner = mlr.learners$get("classif.rpart")
  resampling = mlr.resamplings$get("cv")
  resampling$iters = 3
  measures = list(mlr.measures$get("mmce"))
  rr = resample(task, learner, resampling, measures)
  rr$data
  expect_is(rr, "ResampleResult")
  expect_data_table(rr$data, ncol = 8, nrow = 3)
})
