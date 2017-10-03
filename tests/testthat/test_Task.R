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
    expect_data_table(task$head(1L), types = c("logical", "numeric", "factor", "character"))
  }
})

test_that("Tasks are cloned", {
  task = mlr.tasks$get("bh")
  expect_true(!identical(address(task), address(mlr.tasks$env$bh)))

  data = data.table(x = 1:30, y = factor(sample(letters[1:2], 30, replace = TRUE)))
  task = TaskClassif$new("testthat-example", data, "y")
  mlr.tasks$add(task$clone(deep = TRUE), overwrite = TRUE)
  on.exit(mlr.tasks$remove("testthat-example"))

  rtask = mlr.tasks$get("testthat-example")
  expect_true(!identical(address(task), address(rtask)))
})
