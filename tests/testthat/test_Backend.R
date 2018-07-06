context("Backend")

test_that("BackendDataTable construction", {
  data = iris
  b = BackendDataTable$new(data = data)
  expect_backend(b)
  expect_integer(b$rownames, len = 150, any.missing = FALSE, sorted = TRUE, lower = 1, upper = 150, unique = TRUE)

  data$id = sprintf("r%02i", 1:150)
  b = BackendDataTable$new(data = data, primary.key = "id")
  expect_backend(b)
  expect_character(b$rownames, len = 150, any.missing = FALSE, unique = TRUE, pattern = "^r[0-9]+$")

  rownames(data) = data$id
  data$id = NULL
  b = BackendDataTable$new(data = data)
  expect_backend(b)
  expect_character(b$rownames, len = 150, any.missing = FALSE, unique = TRUE, pattern = "^r[0-9]+$")
})

test_that("BackendDplyr construction", {
  data = iris
  data$id = 1:150
  con <- DBI::dbConnect(RSQLite::SQLite(), path = ":memory:")
  dplyr::copy_to(con, data, "iris")
  data = dplyr::tbl(con, "iris")
  primary.key = "id"
  b = BackendDplyr$new(data, primary.key)

  expect_backend(b)
  expect_data_table(b$head(), nrow = 6, ncol = 6)
  expect_set_equal(b$colnames, c(names(iris), primary.key))
  expect_set_equal(b$rownames, 1:150)
  expect_equal(b$nrow, 150)
  expect_equal(b$ncol, 6)
  expect_data_table(b$data(cols = "Species", rows = 3:10), any.missing = FALSE, nrow = 8, ncol = 1)
})
