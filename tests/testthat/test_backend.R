context("Backend")

test_that("Basic data.table backend ops", {
  data = iris
  data$my.id = sprintf("a%03i", 1:150)
  backends = list(
    DataBackendDataTable$new(data = data, id.col = "my.id"),
    DataBackendDplyr$new(data = asDplyr(data), id.col = "my.id")
  )

  for (b in backends) {
    expect_identical(b$nrow, 150L)
    expect_identical(b$ncol, 5L)
    expect_identical(b$id.col, "my.id")
    expect_data_table(b$get(), nrow = 150, ncol = 5, any.missing = FALSE)
    expect_data_table(b$get(sprintf("a%03i", 1:10)), nrow = 10, ncol = 5)
    expect_data_table(b$get(cols = "Sepal.Length"), nrow = 150, ncol = 1)
    b$active.cols = setdiff(b$active.cols, "Sepal.Length")
    b$active.rows = sprintf("a%03i", 1:49)
    expect_data_table(b$get(), nrow = 49, ncol = 4)

    expect_identical(b$nrow, 49L)
    expect_identical(b$ncol, 4L)
  }
})

test_that("Tasks are cloned", {
  task = Tasks$get("bh")
  expect_true(!identical(address(task), address(Tasks$env$bh)))
  expect_true(!identical(address(task$backend), address(Tasks$env$bh$backend)))

  data = data.table(x = 1:30, y = factor(sample(letters[1:2], 30, replace = TRUE)))
  task = TaskClassif$new(data, target = "y", id = "testthat-example")
  Tasks$add(task$clone(deep = TRUE), overwrite = TRUE)
  on.exit(Tasks$remove("testthat-example"))

  rtask = Tasks$get("testthat-example")
  expect_true(!identical(address(task), address(rtask)))
  expect_true(!identical(address(task$backend), address(rtask$backend)))

  task$backend$subsample(ratio = 0.5)
  expect_identical(rtask$backend$nrow, 30L)

  task = Tasks$get("sonar")
  task$backend$subsample(ratio = 0.5)
  expect_identical(Tasks$get("sonar")$backend$nrow, 208L)
})

test_that("getting ids", {
  task = Tasks$get("iris")
  expect_identical(task$backend$active.rows[1:20], 1:20)

  task = asDplyrTask(task)
  expect_identical(task$backend$active.rows[1:20], 1:20)
  expect_data_table(task$backend$get(cols = "Species"), ncol = 1, nrow = 150)
})
