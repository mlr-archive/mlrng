context("benchmark")

test_that("benchmark", {
  tasks = lapply(c("iris", "sonar"), mlr.tasks$get)
  lrn1 = mlr.learners$get("classif.rpart")
  lrn1$par.vals = list(cp = 0.01)
  lrn2 = mlr.learners$get("classif.dummy")
  learners = list(lrn1, lrn2)
  resamplings = list(mlr.resamplings$get("cv"))
  measures = list(mlr.measures$get("mmce"))

  bmr = benchmark(tasks, learners, resamplings, measures)
  expect_data_table(bmr$data, ncols = 7, nrows = 40)
})
