context("benchmark")

test_that("benchmark", {
  tasks = lapply(c("iris", "sonar"), Tasks$get)
  lrn1 = Learners$get("classif.rpart")
  lrn1$par.vals = list(mtry = 2)
  lrn2 = Learners$get("classif.dummy")
  learners = list(lrn1, lrn2)
  resamplings = list(Resamplings$get("cv"))
  measures = list(Measures$get("mmce"))

  bmr = benchmark(tasks, learners, resamplings, measures)
  expect_data_table(bmr$data, ncol = 8, nrow = 40)
})
