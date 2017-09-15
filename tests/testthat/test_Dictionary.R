context("Dictionary")

test_that("Basic ops on Dictionary", {
  d = Dictionary$new("numeric")
  expect_equal(d$length, 0L)
  expect_message(summary(d), "of 0 numeric")

  d$add(1, id = "x")
  expect_message(summary(d), "of 1 numeric")
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
  dd$restrict("y")
  expect_equal(as.list(dd), list(y = 2))
})

