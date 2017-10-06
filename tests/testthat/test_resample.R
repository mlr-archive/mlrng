context("resample")

test_that("Basic resampling", {
  task = mlr.tasks$get("iris")
  learner = mlr.learners$get("classif.rpart")
  resampling = mlr.resamplings$get("cv")
  resampling$iters = 3
  measures = list(mlr.measures$get("mmce"))
  resampling$instantiate(task)
  rr = resample(task, learner, resampling, measures)
  expect_result(rr)
  expect_data_table(rr$data, nrow = 3)
})
