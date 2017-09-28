context("resample")

test_that("runExperiment worker", {
  task = Tasks$get("iris")
  learner = Learners$get("classif.rpart")
  resampling = Resamplings$get("cv")
  resampling$instantiate(task)
  resampling.iter = 1L
  measures = list(Measures$get("mmce"))

  res = list()
  for (i in 1:10) {
    res[[i]] = runExperiment(task, learner, resampling, i, measures)
  }
  rr = rbindlist(res)
  res = resample(task, learner, resampling, measures)
  expect_is(res, "ResampleResult")
})

test_that("Basic resampling", {
  task = Tasks$get("iris")
  learner = Learners$get("classif.rpart")
  resampling = Resamplings$get("holdout")
  measures = list(Measures$get("mmce"))

  res = resample(task, learner, resampling, measures)
  expect_is(res, "ResampleResult")
})
