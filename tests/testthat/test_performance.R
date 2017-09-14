context("performance")

test_that("performance", {
  task = getTask("iris")
  learner = getLearner("classif.dummy", method = "sample")
  model = train(task, learner)
  pred = predict(model, task)
  p = performance(pred, measures = getMeasures("mmce"))
  expect_numeric(p, len = 1L, names = "unique" )
  expect_named(p, "mmce")
})

