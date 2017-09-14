context("benchmark")

test_that("benchmark", {
  tasks = list("iris", "sonar")
  learners = list(getLearner("classif.rpart", mtry = 2), getLearner("classif.dummy"))
  resamplings = "cv"
  measures = "mmce"

  bmr = benchmark(tasks, learners, resamplings, measures)
  expect_data_table(bmr$data, nrow = 40)
  expect_data_table(as.data.table(bmr), nrow = 40)
})
