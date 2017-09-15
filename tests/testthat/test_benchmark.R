context("benchmark")

test_that("benchmark", {
  tasks = lapply(c("iris", "sonar"), Tasks$get)
  learners = list(getLearner("classif.rpart", mtry = 2), getLearner("classif.dummy"))
  resamplings = list(Resamplings$get("cv"))
  measures = list(Measures$get("mmce"))

  bmr = benchmark(tasks, learners, resamplings, measures)
  expect_data_table(bmr$data, nrow = 40)
  expect_data_table(as.data.table(bmr), nrow = 40)
})
