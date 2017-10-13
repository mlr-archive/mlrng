context("PredictResult")

test_that("PredictResult$pred works", {
  lrn = mlr.learners$get("classif.dummy")
  task = test.tasks$get("clm.num")
  m = train(task, lrn)
  p = predict(m)
  pdt = p$pred
  expect_data_table(pdt, nrow = task$nrow, ncol = 3L, types = c("integer", "character", "factor")) # FIXME: Should be same type!
  expect_names(names(pdt), identical.to = c("test.set", "truth", "response"))

  lrn = mlr.learners$get("regr.dummy")
  task = test.tasks$get("regr.num")
  m = train(task, lrn)
  p = predict(m)
  pdt = p$pred
  expect_data_table(pdt, nrow = task$nrow, ncol = 3L, types = c("integer", "numeric"))
  expect_names(names(pdt), identical.to = c("test.set", "truth", "response"))
})
