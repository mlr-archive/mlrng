context("Backend")

test_that("Basic data.table backend ops", {
  task = getTask("iris")

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
  expect_identical(getTask("iris")$nrow, 150L)
})

test_that("Tasks are cloned", {
  task = Tasks$get("bh")
  expect_true(!identical(address(task), address(Tasks$storage$bh)))
  expect_true(!identical(address(task$backend), address(Tasks$storage$bh$backend)))


  data = data.table(x = 1:30, y = factor(sample(letters[1:2], 30, replace = TRUE)))
  task = ClassifTask$new(data, target = "y", id = "testthat-example")
  Tasks$register(task)
  on.exit(Tasks$remove("testthat-example"))
  rtask = Tasks$get("testthat-example")
  expect_true(!identical(address(task), address(rtask)))
  expect_true(!identical(address(task$backend), address(rtask$backend)))

  task$backend$subsample(ratio = 0.5)
  expect_identical(rtask$nrow, 30L)

  task = getTask("sonar")
  task$backend$subsample(ratio = 0.5)
  expect_identical(getTask("sonar")$nrow, 208L)
})

test_that("getting ids", {
  task = getTask("iris")
  expect_identical(task$backend$ids(1:20), 1:20)

  task = asDplyrTask(task)
  task$backend$get(cols = "Species")
})

test_that("id columns work", {
  task = getTask("breastcancer")
  task$backend$ids(1:10)

  mod = train(task, "classif.dummy")
  predict(mod, task)
})
