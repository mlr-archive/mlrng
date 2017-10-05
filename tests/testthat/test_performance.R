context("performance")

test_that("performance", {
  task = mlr.tasks$get("iris")
  learner = mlr.learners$get("classif.dummy")
  model = train(task, learner)
  pred = predict(model, task, subset = 1:150)
  p = performance(pred, measures = list(mlr.measures$get("mmce")))
  pv = p$perf.vals
  expect_numeric(pv, len = 1L, names = "unique", finite = TRUE, any.missing = FALSE)
  expect_named(pv, "mmce")
})
