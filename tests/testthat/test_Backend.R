context("Backends")

test.data = iris
test.data$na.col = replace(runif(150), 76:125, NA)
test.data$int.col = as.double(sample(0:1, 150, replace = TRUE))


expect_test_data = function(b) {
  cols = names(test.data)
  expect_equal(b$nrow, 150)
  expect_equal(b$ncol, length(cols) + 1L)
  expect_set_equal(b$rownames, 1:150)
  expect_set_equal(b$colnames, c(cols, b$rowid.col))
  expect_data_table(b$data, nrow = 150, ncol = length(cols) + 1L)
  expect_character(b$distinct("Species"), len = 3, any.missing = FALSE)
  expect_set_equal(b$distinct("Species"), levels(test.data$Species))
  expect_data_table(b$get(), nrow = 150, ncol = length(cols) + 1L)
  expect_data_table(b$get(rows = 5, "Species"), nrow = 1, ncol = 1)
  expect_data_table(b$head(3), nrow = 3, ncol = length(cols) + 1L)
  # expect_identical(b$missing.values("Species"), 0L)
  # expect_identical(b$missing.values("na.col"), 50L)
}

test_that("SQLite backend", {
  b = BackendDBI$new(test.data, tbl.name = "iris")
  expect_backend(b)
  expect_test_data(b)
})

test_that("Local (in-memory) backend", {
  b = BackendLocal$new(test.data)
  expect_backend(b)
  expect_test_data(b)
})


test_that("converters work", {
  b = BackendDBI$new(test.data, tbl.name = "iris")

  b$converters = list(
    Species = function(x) factor(x),
    int.col = function(x) as.integer(x)
  )

  data = b$head(1)
  expect_data_table(data, types = c("integer", "numeric", "factor"))
  expect_factor(data$Species, any.missing = FALSE)
  expect_integer(data$int.col, any.missing = FALSE)

  data = b$data
  expect_data_table(data, types = c("integer", "numeric", "factor"))
  expect_factor(data$Species, any.missing = FALSE)
  expect_integer(data$int.col, any.missing = FALSE)

  data = b$get()
  expect_data_table(data, types = c("integer", "numeric", "factor"))
  expect_factor(data$Species, any.missing = FALSE)
  expect_integer(data$int.col, any.missing = FALSE)

  data = b$distinct("Species")
  expect_character(data, len = 3L, any.missing = FALSE)

  data = b$distinct("int.col")
  expect_integer(data, len = 2L, lower = 0L, upper = 1L, any.missing = FALSE)

  expect_set_equal(b$types, c("integer", "numeric", "factor"))
})
