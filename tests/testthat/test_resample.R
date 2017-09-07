context("resample")

test_that("Basic resampling", {
  learner = getLearner("classif.rpart", mtry = 2)
  learner = downsampleWrapper(learner, ratio = 0.1)
  rr = resample("iris", learner, "cv", "mmce")
  expect_class(rr, "ResampleResult")
  expect_list(rr$data, len = 10, names = "unnamed")
  expect_data_table(as.data.table(rr), nrow = 10, ncol = 4)
})
