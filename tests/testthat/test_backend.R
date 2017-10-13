context("Backends")

test_that("SQLite backend", {
  b = BackendDBI$new(iris, tbl.name = "iris")
  expect_backend(b)
})

test_that("Local (in-memory) backend", {
  b = BackendLocal$new(iris)
  expect_backend(b)
})
