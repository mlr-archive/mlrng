context("Backends")

test.data = iris
test.data$na.col = replace(runif(150), 76:125, NA)
test.data$int.col = as.double(sample(0:1, 150, replace = TRUE))

expect_transformators_work = function(b) {
  b$transformators = list(
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

  b$transformators = list()
}

expect_subsetting_works = function(b) {
  cols = c(names(iris), c("na.col", "int.col"))

  expect_equal(b$nrow, 150)
  expect_equal(b$ncol, length(cols))
  expect_set_equal(b$rownames, 1:150)
  expect_set_equal(b$colnames, cols)
  expect_data_table(b$data, nrow = 150, ncol = length(cols))
  expect_character(b$distinct("Species"), len = 3, any.missing = FALSE)
  expect_set_equal(b$distinct("Species"), levels(iris$Species))
  expect_data_table(b$get(), nrow = 150, ncol = length(cols))
  expect_data_table(b$get(rows = 5, "Species"), nrow = 1, ncol = 1)
  expect_data_table(b$get(include.rowid.col = TRUE), nrow = 150, ncol = length(cols) + 1L)
  expect_data_table(b$head(3), nrow = 3, ncol = length(cols))
  x = b$missing.values
  expect_equal(x[match(cols, names(x), 0L)], setNames(c(0, 0, 0, 0, 0, 50, 0), cols))
  x = b$types
  expect_equal(x[match(cols, names(x), 0L)], setNames(c("numeric", "numeric", "numeric", "numeric", "factor", "numeric", "numeric"), cols))

  b$subset(sample(11:100), setdiff(b$colnames, "Petal.Length"))

  cols = c(setdiff(names(iris), "Petal.Length"), c("na.col", "int.col"))
  expect_equal(b$nrow, 90)
  expect_equal(b$ncol, length(cols))
  expect_set_equal(b$rownames, 11:100)
  expect_set_equal(b$colnames, cols)
  expect_data_table(b$data, nrow = 90, ncol = length(cols))
  expect_character(b$distinct("Species"), len = 2, any.missing = FALSE)
  expect_set_equal(b$distinct("Species"), c("setosa", "versicolor"))
  expect_data_table(b$get(), nrow = 90, ncol = length(cols))
  expect_error(b$get(rows = 5, "Species"), "Invalid row ids")
  expect_data_table(b$get(include.rowid.col = TRUE), nrow = 90, ncol = length(cols) + 1L)
  expect_data_table(b$head(3), nrow = 3, ncol = length(cols))
  x = b$missing.values
  expect_equal(x[match(cols, names(x), 0L)], setNames(c(0, 0, 0, 0, 25, 0), cols))
  x = b$types
  expect_equal(x[match(cols, names(x), 0L)], sapply(test.data[cols], class))

  b$subset(1:150, cols = c(names(iris), c("na.col", "int.col")))
}

test_that("SQLite backend", {
  b = BackendDBI$new(test.data, tbl.name = "iris")
  expect_backend(b)
  expect_subsetting_works(b)
  expect_transformators_work(b)
})

test_that("Local (in-memory) backend", {
  b = BackendLocal$new(test.data)
  expect_backend(b)
  expect_subsetting_works(b)
  expect_transformators_work(b)
})
