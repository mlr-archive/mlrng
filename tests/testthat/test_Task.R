context("Task")


test_that("Task Construction", {
  task = Task$new(iris)
  expect_task(task)
  expect_is(task$backend, "DataTableBackend")

  task = Task$new(as.data.table(iris))
  expect_task(task)
  expect_is(task$backend, "DataTableBackend")

  requireNamespace("tibble")
  task = Task$new(tibble::as_tibble(iris))
  expect_task(task)
  expect_is(task$backend, "DataTableBackend")
})

test_that("Example Tasks", {
  for (id in Tasks$ids) {
    task = getTask(id)
    expect_data_table(task[], types = c("numeric", "factor"))
  }
})

test_that("Tasks rows are accessed integers", {
  id = function(x) data.table(..id = as.integer(x))
  task = getTask("iris")
  task$backend$slice(20:25)
  expect_data_table(task$backend[id(20), ], nrow = 1, ncol = 5, any.missing = FALSE)
  expect_data_table(task$backend[20, ], nrow = 1, ncol = 5, any.missing = FALSE)
  expect_data_table(task$backend[id(20:25), ], nrow = 6, ncol = 5, any.missing = FALSE)
  expect_data_table(task$backend[20:25, ], nrow = 6, ncol = 5, any.missing = FALSE)
  expect_data_table(task$backend[id(1L), ], nrow = 0, ncol = 5, any.missing = FALSE)
  expect_data_table(task$backend[1L, ], nrow = 0, ncol = 5, any.missing = FALSE)

  expect_data_table(task[1, ], nrow = 1, ncol = 5, any.missing = FALSE)
  expect_data_table(task[1:6, ], nrow = 6, ncol = 5, any.missing = FALSE)
  expect_data_table(task[50, ], nrow = 0, ncol = 5, any.missing = FALSE)
})
