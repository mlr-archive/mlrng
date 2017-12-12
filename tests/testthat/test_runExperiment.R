context("runExperiment")

test_that("runExperiment worker", {
  task = test.tasks$get("clm.num")
  learner = mlr.learners$get("classif.rpart")
  resampling = mlr.resamplings$get("cv")
  resampling$instantiate(task)
  measures = list(mlr.measures$get("mmce"))
  for (i in 1:2) {
    r = runExperiment(task, learner, resampling, i, measures, store.model = FALSE)
    expect_r6(r, "PerformanceResult")
    expect_result(r)
  }
})
