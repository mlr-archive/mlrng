context("Task")

con = ConnectionMem$new("foo", data = iris)

test_that("Task Construction", {
  task = Task$new(id = "foo", con)
  expect_task(task)
})

test_that("TaskSupervised Construction", {
  task = TaskSupervised$new(id = "foo", con, target = "Species")
  expect_supervisedtask(task)
})

test_that("Registered Tasks are valid", {
  for (id in Tasks$ids) {
    task = Tasks$get(id)
    expect_task(task)
    expect_data_table(task$head(1), types = c("logical", "numeric", "factor", "character"))
  }
})

test_that("Tasks are cloned", {
  task = Tasks$get("bh")
  expect_true(!identical(address(task), address(Tasks$env$bh)))

  data = data.table(x = 1:30, y = factor(sample(letters[1:2], 30, replace = TRUE)))
  task = TaskClassif$new("testthat-example", ConnectionMem$new("testthat-example", data), "y")
  Tasks$add(task$clone(deep = TRUE), overwrite = TRUE)
  on.exit(Tasks$remove("testthat-example"))

  rtask = Tasks$get("testthat-example")
  expect_true(!identical(address(task), address(rtask)))
})
