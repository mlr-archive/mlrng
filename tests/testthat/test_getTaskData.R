context("getTaskData")

types = mlrng$supported.col.types
props = mlrng$supported.learner.props

test_that("convertFeatures", {
  x = data.table(log = replace(logical(10), 5:10, TRUE), int = 1:10, real = runif(10), char = letters[1:10], fac = factor(letters[1:10]))

  expect_identical(convertFeatures(copy(x), NULL), x)
  expect_data_table(convertFeatures(copy(x), props), types = types)

  y = convertFeatures(copy(x), c("feat.logical", "feat.integer", "feat.character"))
  expect_data_table(y, types = setdiff(types, "factor"))

  y = convertFeatures(copy(x), c("feat.logical", "feat.integer", "feat.factor"))
  expect_data_table(y, types = setdiff(types, "character"))

  y = convertFeatures(copy(x), c("feat.integer", "feat.character", "feat.factor"))
  expect_data_table(y, types = setdiff(types, "logical"))

  y = convertFeatures(copy(x), c("feat.character", "feat.factor"))
  expect_data_table(y, types = setdiff(types, c("logical", "integer")))
})

test_that("getTaskData", {
  x = data.table(log = replace(logical(10), 5:10, TRUE), int = 1:10, real = runif(10), char = letters[1:10], fac = factor(letters[1:10]))
  x$y = rep(letters[1:2], each = 5)
  task = TaskSupervised$new("testtask", x, target = "y")

  expect_data_table(getTaskData(task, 1:5, "train"), nrows = 5, ncols = 6, any.missing = FALSE)
  expect_data_table(getTaskData(task, 1:5, "test"), nrows = 5, ncols = 5, any.missing = FALSE)
  expect_data_table(getTaskData(task, 1:5, "test", "feat.factor"), nrows = 5, ncols = 5, types = setdiff(types, "character"))

  y = getTaskData(task, 1:5, "extra", "feat.character")
  expect_list(y, len = 2)
  expect_set_equal(names(y), c("x", "y"))
  expect_data_table(y$y, ncols = 1)
  expect_data_table(y$x, nrows = 5, ncols = 5, types = setdiff(types, "factor"))

  y = getTaskData(task, 1:5, "extra", "feat.character", target.as = "character")
  expect_character(y$y[[task$target]])
  y = getTaskData(task, 1:5, "extra", "feat.character", target.as = "factor")
  expect_factor(y$y[[task$target]])
})
