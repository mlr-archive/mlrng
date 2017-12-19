context("Task")

test_that("Task Construction", {
  task = Task$new(id = "foo", iris)
  b = task$backend
  expect_task(task)

  data = iris
  data$..row.id = 1:150
  b = BackendDBI$new(data, rowid.col = "..row.id", tbl.name = "iris")
  expect_task(task)

  new.task = task$clone()
  b = BackendLocal$new(task$backend$data, task$backend$rowid.col)
  new.task$backend = b
  new.task$backend$internal.data$Sepal.Length = 1
  expect_task(new.task)
})

test_that("TaskSupervised Construction", {
  task = TaskSupervised$new(id = "foo", iris, target = "Species")
  expect_supervisedtask(task)
})

test_that("Registered Tasks are valid", {
  for (id in mlr.tasks$ids) {
    task = mlr.tasks$get(id)
    expect_supervisedtask(task)
  }
})

test_that("Tasks are cloned", {
  task1 = mlr.tasks$get("bh")
  task2 = task1$clone(deep = TRUE)

  expect_different_address(task1, task2)
  expect_same_address(task1$backend, task2$backend)

  data = data.table(x = 1:30, y = factor(sample(letters[1:2], 30, replace = TRUE)))
  task = TaskSupervised$new("testthat-example", data, "y")
  task = TaskClassif$new("testthat-example", data, "y")
  mlr.tasks$add(task$clone(deep = TRUE), overwrite = TRUE)
  on.exit(mlr.tasks$remove("testthat-example"))

  rtask = mlr.tasks$get("testthat-example")
  expect_different_address(task, rtask)
  expect_same_address(task$backend, rtask$backend)
})

test_that("Tasks can be loaded from the fs", {
  task = TaskClassif$new(id = "iris", data = iris, "Species")
  fn.rds = tempfile(fileext = ".rds")
  saveRDS(task, file = fn.rds)
  rm(task)

  task = readRDS(fn.rds)
  expect_supervisedtask(task)
})

test_that("Task subsetting works", {
  task = mlr.tasks$get("iris")
  expect_identical(task$nrow, 150L)
  nt = task$clone(TRUE)
  nt$row.roles[id %in% 31:50, role := "ignore"]
  expect_task(nt)
  expect_different_address(task, nt)
  expect_identical(nt$nrow, 130L)
  expect_identical(task$nrow, 150L)

  # re-grow the task
  nt$row.roles[, role := "training"]
  expect_identical(nt$nrow, 150L)
})

test_that("Task$truth works", {
  task = TaskClassif$new(id = "iris", data = iris, target = "Species")
  y = task$truth()
  expect_data_table(y, nrow = task$nrow, ncol = 1L, types = "factor")
  expect_names(names(y), identical.to = task$target)

  y = task$truth(1:10L)
  expect_data_table(y, nrow = 10L, ncol = 1L, types = "factor")
  expect_names(names(y), identical.to = task$target)
})

test_that("Task$get() returns duplicated rows", {
  tasks = list(test.tasks$get("regr.num"), test.tasks$get("clm.num"))
  for (task in tasks) {
    ids = rep(head(task$backend$rownames, 6), 2)
    x = task$get(rows = ids)
    expect_data_table(x, nrow = length(ids))
    expect_identical(x[1:6], x[7:12])
    x = task$backend$get(rows = ids, cols = task$col.roles$id)
    expect_identical(x[[task$backend$rowid.col]], ids)
  }
})

test_that("Tasks are auto-converted on change", {
  task = mlr.tasks$get("iris")
  expect_class(task$backend, "BackendDBI")
  newdata = task$data
  newdata[[task$backend$rowid.col]] = task$backend$rownames
  task$data = newdata[1:15]
  expect_class(task$backend, "BackendLocal")
  expect_data_table(task$data, nrow = 15, ncol = 5, any.missing = FALSE)
})
