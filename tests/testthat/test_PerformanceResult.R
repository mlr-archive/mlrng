context("PerformanceResult")

test_that("PerformanceResult is valid", {
  lrn = mlr.learners$get("classif.dummy")
  task = test.tasks$get("clm.num")
  m = train(task, lrn)
  pr = predict(m)
  qr = performance(pr, measures = list(mlr.measures$get("mmce")))

  expect_predictresult(qr)

  lrn = mlr.learners$get("regr.dummy")
  task = test.tasks$get("regr.num")
  m = train(task, lrn)
  pr = predict(m)
  expect_predictresult(pr)
})
