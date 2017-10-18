context("predict")

test_that("Step by step modeling", {
  task = mlr.tasks$get("iris")
  learner = mlr.learners$get("classif.dummy")
  train = sample(150, 120)
  model = train(task, learner, subset = train)
  expect_is(model, "TrainResult")
  expect_result(model)
  pred = predict(model, subset = setdiff(1:150, train))
  expect_character(pred$predicted, len = 30, any.missing = FALSE)
  expect_subset(pred$predicted, levels(iris$Species))

  train = sample(150, 100)
  test = setdiff(seq_len(150), train)
  learner = mlr.learners$get("classif.rpart")
  model = train(task, learner, subset = train)
  expect_is(model, "TrainResult")
  expect_result(model)

  pred = predict(model, subset = test)
  expect_result(pred)
  expect_character(pred$predicted, len = length(test), any.missing = FALSE)
  expect_subset(pred$predicted, levels(iris$Species))

  train = 1:150
  test = integer(0)
  learner = mlr.learners$get("classif.rpart")
  model = train(task, learner, subset = train)
  expect_is(model, "TrainResult")
  pred = predict(model, subset = test)
  expect_character(pred$predicted, len = length(test), any.missing = FALSE)
  expect_subset(pred$predicted, levels(iris$Species))
})
