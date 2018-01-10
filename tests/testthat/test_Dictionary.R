context("Dictionary")

test_that("Dictionary with R6 els", {
  Foo = R6::R6Class("Foo", public = list(x=0, initialize = function(x) self$x = x), cloneable = TRUE)
  d = Dictionary$new("Foo")
  expect_identical(d$eltype, "Foo")
  expect_identical(d$length, 0L)
  expect_identical(d$ids, character(0L))
  expect_output(print(d), "of 0 Foo")

  f1 = Foo$new(1)
  f2 = Foo$new(2)

  d$add(f1, id = "x")
  expect_identical(d$length, 1L)
  expect_identical(d$ids, "x")
  expect_output(print(d), "of 1 Foo")
  expect_data_table(d$summary("x"), nrow = 1L)
  f1c = d$get("x")
  expect_different_address(f1, f1c)

  xs = as.list(d)
  expect_equal(xs, list(x = f1))

  xd = as.data.table(d)
  expect_data_table(xd, nrow = 1L, min.cols = 2L)

  d$add(f2, id = "y")
  expect_identical(d$length, 2L)
  expect_set_equal(d$ids, c("y", "x"))

  d$add(f2, id = "y")
  expect_identical(d$length, 2L)

  d$slice("y")
  expect_set_equal(d$ids, "y")
})
