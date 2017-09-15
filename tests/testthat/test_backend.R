context("Backend")

test_that("Basic data.table backend ops", {
  task = Tasks$get("iris")

  for (task in list(task, asDplyrTask(task))) {
    b = task$backend
    expect_identical(b$nrow, 150L)
    expect_identical(b$ncol, 5L)
    expect_identical(b$id.col, "..id")
    expect_data_table(b$get(), nrow = 150, ncol = 5, any.missing = FALSE)
    expect_data_table(b$get(data.table(..id = 1:10)), nrow = 10, ncol = 5)
    expect_data_table(b$get(cols = "Sepal.Length"), nrow = 150, ncol = 1)
    b$drop("Sepal.Length")
    b$slice(1:49)
    expect_data_table(b$get(), nrow = 49, ncol = 4)

    expect_identical(b$nrow, 49L)
    expect_identical(b$ncol, 4L)
  }

  # example task still has same dimensions?
  expect_identical(Tasks$get("iris")$nrow, 150L)
})

test_that("Tasks are cloned", {
  task = Tasks$get("bh")
  expect_true(!identical(address(task), address(Tasks$storage$bh)))
  expect_true(!identical(address(task$backend), address(Tasks$storage$bh$backend)))


  data = data.table(x = 1:30, y = factor(sample(letters[1:2], 30, replace = TRUE)))
  task = ClassifTask$new(data, target = "y", id = "testthat-example")
  Tasks$add(task)
  on.exit(Tasks$remove("testthat-example"))
  rtask = Tasks$get("testthat-example")
  expect_true(!identical(address(task), address(rtask)))
  expect_true(!identical(address(task$backend), address(rtask$backend)))

  task$backend$subsample(ratio = 0.5)
  expect_identical(rtask$nrow, 30L)

  task = Tasks$get("sonar")
  task$backend$subsample(ratio = 0.5)
  expect_identical(Tasks$get("sonar")$nrow, 208L)
})

test_that("getting ids", {
  task = Tasks$get("iris")
  expect_identical(task$backend$ids(1:20), 1:20)

  task = asDplyrTask(task)
  task$backend$get(cols = "Species")
})

test_that("id columns work", {
  task = Tasks$get("breastcancer")
  task$backend$ids(1:10)

  lrn = Learners$get("classif.dummy")
  mod = train(task, lrn)
  predict(mod, task)
})
