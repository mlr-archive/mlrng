context("resample")

test_that("Basic resampling", {
  task = Tasks$get("iris")
  learner = Learners$get("classif.rpart")
  resampling = Resamplings$get("holdout")
  measures = list(Measures$get("mmce"))

  res = resample(task, learner, resampling, measures)
  expect_is(res, "ResampleResult")
})
