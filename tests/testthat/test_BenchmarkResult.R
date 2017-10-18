context("BenchmarkResult")

test_that("BenchmarkResult is valid", {
  tasks = list(test.tasks$get("clm.num"))
  learners = list(mlr.learners$get("classif.dummy"))
  resamplings = list(mlr.resamplings$get("cv"))
  resamplings[[1]]$iters = 3L
  measures = list(mlr.measures$get("mmce"))
  bmr = benchmark(tasks, learners, resamplings, measures)

  expect_benchmarkresult(bmr)
})
