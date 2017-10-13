context("Task")

test_that("Task Construction", {
  task = Task$new(id = "foo", iris)
  expect_task(task)
})

test_that("TaskSupervised Construction", {
  task = TaskSupervised$new(id = "foo", iris, target = "Species")
  expect_supervisedtask(task)
})

test_that("Registered Tasks are valid", {
  for (id in mlr.tasks$ids) {
    task = mlr.tasks$get(id)
    expect_supervisedtask(task)
    expect_data_table(task$head(1L), types = mlrng$supported.col.types)
  }
})

test_that("Tasks are cloned", {
  task1 = mlr.tasks$get("bh")
  task2 = task1$clone(deep = TRUE)

  expect_different_address(task1, task2)
  expect_different_address(task1$backend, task2$backend)

  data = data.table(x = 1:30, y = factor(sample(letters[1:2], 30, replace = TRUE)))
  task = TaskSupervised$new("testthat-example", data, "y")
  task = TaskClassif$new("testthat-example", data, "y")
  mlr.tasks$add(task$clone(deep = TRUE), overwrite = TRUE)
  on.exit(mlr.tasks$remove("testthat-example"))

  rtask = mlr.tasks$get("testthat-example")
  expect_different_address(task, rtask)
  expect_different_address(task$backend, rtask$backend)
})

test_that("Tasks can be loaded from the fs", {
  task = TaskClassif$new(id = "iris", data = iris, "Species")
  fn.rds = tempfile(fileext = ".rds")
  saveRDS(task, file = fn.rds)
  rm(task)

  task = readRDS(fn.rds)
  expect_supervisedtask(task)
})

test_that("Task$subset works", {
  task = TaskClassif$new(id = "iris", data = iris, target = "Species")
  expect_identical(task$nrow, 150L)
  nt = task$clone(TRUE)$subset(1:90)

  expect_task(nt)
  expect_different_address(task, nt)
  expect_identical(task$nrow, 150L)
  expect_identical(nt$nrow, 90L)
})

test_that("Task$truth works", {
  task = TaskClassif$new(id = "iris", data = iris, target = "Species")
  y = task$truth()
  expect_data_table(y, nrow = task$nrow, ncol = 1L, types = "character")
  expect_names(names(y), identical.to = task$target)

  y = task$truth(1:10L)
  expect_data_table(y, nrow = 10L, ncol = 1L, types = "character")
  expect_names(names(y), identical.to = task$target)
})

# test_that("Task change formula", {
#   task = TaskClassif$new(id = "iris", data = iris, target = "Species")
#   expect_set_equal(all.vars(task$formula), colnames(iris))

#   task$formula = Species ~ Petal.Length
#   expect_set_equal(all.vars(task$formula), c("Species", "Petal.Length"))
# })
