context("Dictionary")

test_that("Basic ops on Dictionary, no R6 els", {
  d = Dictionary$new("numeric")
  expect_equal(d$length, 0L)
  expect_message(print(d), "of 0 numeric")

  d$add(1, id = "x")
  expect_equal(d$length, 1L)
  expect_error(d$add(1, id = "x"), "already present")
  expect_error(d$add("x", id = "x"), "numeric")
  xs = as.list(d)
  expect_equal(xs, list(x = 1))
  d1 = d$clone(deep = TRUE)

  d$add(2, id = "y")
  expect_equal(d$length, 2L)
  xs = as.list(d)
  expect_equal(xs, list(x = 1, y = 2))
  expect_equal(d1$length, 1L) # make sure after cloning
  dd = d$clone(deep = TRUE)
  dd$slice("y")
  expect_equal(as.list(dd), list(y = 2))
})

test_that("Dictionary with R6 els", {
  Foo = R6Class("Foo", public = list(x=0, initialize = function(x) self$x = x), cloneable = TRUE)
  d = Dictionary$new("Foo")
  expect_equal(d$length, 0L)
  expect_message(print(d), "of 0 Foo")
  f1 = Foo$new(1)
  f2 = Foo$new(2)

  d$add(f1, id = "x")
  expect_equal(d$length, 1L)
  xs = as.list(d)
  expect_equal(xs, list(x = f1))
  d1 = d$clone(deep = TRUE)
})


