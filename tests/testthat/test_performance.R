context("performance")

test_that("performance", {
  task = Tasks$get("iris")
  learner = Learners$get("classif.dummy")
  model = train(task, learner)
  pred = predict(model, task, subset = 1:150)
  p = performance(pred, measures = list(Measures$get("mmce")))
  expect_numeric(p, len = 1L, names = "unique" )
  expect_named(p, "mmce")
})
