context("resample")

test_that("Basic resampling", {
  task = mlr.tasks$get("iris")
  learner = mlr.learners$get("classif.rpart")
  resampling = mlr.resamplings$get("cv")
  resampling$iters = 3
  measures = list(mlr.measures$get("mmce"))
  resampling$instantiate(task)
  expect_r6dt2d(rr, "ResampleResult", nrow = 3L)
})
