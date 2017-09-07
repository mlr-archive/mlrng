context("benchmark")

test_that("benchmark", {
  tasks = list("iris", "sonar")
  learners = list(getLearner("classif.rpart", mtry = 2), getLearner("classif.dummy"))
  resamplings = "cv"
  measures = "mmce"

  bmr = benchmark(tasks, learners, resamplings, measures)
  bmr$data
})
