context("Backend")

test_that("Basic backend ops", {
  data = iris
  rowid.col = "my.id"
  data[[rowid.col]] = sprintf("a%03i", 1:150)
  backends = list(
    DataBackendDataTable$new(data = data, rowid.col = rowid.col),
    DataBackendDplyr$new(data = asDplyr(data), rowid.col = rowid.col)
  )

  for (b in backends) {
    expect_identical(b$nrow, 150L)
    expect_identical(b$ncol, 5L)
    expect_identical(b$rowid.col, rowid.col)
    expect_data_table(b$get(), nrow = 150, ncol = 5, any.missing = FALSE)
    expect_data_table(b$get(sprintf("a%03i", 1:10)), nrow = 10, ncol = 5)
    expect_data_table(b$get(cols = "Sepal.Length"), nrow = 150, ncol = 1)
    b$active.cols = setdiff(b$active.cols, "Sepal.Length")
    b$active.rows = sprintf("a%03i", 1:49)
    expect_data_table(b$get(), nrow = 49, ncol = 4)
    expect_identical(b$nrow, 49L)
    expect_identical(b$ncol, 4L)

    expect_set_equal(b$active.cols, setdiff(names(iris), "Sepal.Length"))
    expect_set_equal(b$active.rows, data[[rowid.col]][1:49])
    expect_set_equal(b$all.cols, names(data))
    expect_set_equal(b$all.rows, data[[rowid.col]])
  }

  task = Tasks$get("iris")
  expect_identical(task$backend$active.rows[1:20], 1:20)

  task = asDplyrTask(task)
  expect_identical(task$backend$active.rows[1:20], 1:20)
  expect_data_table(task$backend$get(cols = "Species"), ncol = 1, nrow = 150)
})
