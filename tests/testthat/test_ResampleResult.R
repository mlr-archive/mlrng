context("ResampleResult")

test_that("can create from runExperiment", {
  task = test.tasks$get("clm.num")
  learner = mlr.learners$get("classif.rpart")
  resampling = mlr.resamplings$get("cv")
  resampling$instantiate(task)
  measures = list(mlr.measures$get("mmce"))
  rs = lapply(1:2, runExperiment, task = task, learner = learner, resampling = resampling, measures = measures)
  rr = ResampleResult$new(rs)
  expect_result(rr)
  expect_data_table(rr$data, nrow = 2)
})
