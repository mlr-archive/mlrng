context("Backends")

expect_transformators_work = function(b) {
  b$transformators = list(Species = function(x) factor(x))
  data = b$head(1)
  expect_data_table(data, types = c("numeric", "factor"))
  expect_factor(data$Species, any.missing = FALSE)

  data = b$data
  expect_data_table(data, types = c("numeric", "factor"))
  expect_factor(data$Species, any.missing = FALSE)

  data = b$get()
  expect_data_table(data, types = c("numeric", "factor"))
  expect_factor(data$Species, any.missing = FALSE)
}

test_that("SQLite backend", {
  b = BackendDBI$new(iris, tbl.name = "iris")
  expect_backend(b)
  expect_transformators_work(b)
})

test_that("Local (in-memory) backend", {
  b = BackendLocal$new(iris)
  expect_backend(b)
  expect_transformators_work(b)
})
