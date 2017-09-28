context("Task")

test_that("Task Construction", {
  task = Task$new(id = "foo", data = iris)
  expect_task(task)
})

test_that("TaskSupervised Construction", {
  task = TaskSupervised$new(id = "foo", data = iris, target = "Species")
  expect_supervisedtask(task)
})

test_that("Example Tasks", {
  for (id in Tasks$ids) {
    task = Tasks$get(id)
    expect_task(task)
    expect_data_table(task$backend$get(), types = c("logical", "numeric", "factor"))
  }
})

test_that("Tasks are cloned", {
  task = Tasks$get("bh")
  expect_true(!identical(address(task), address(Tasks$env$bh)))
  expect_true(!identical(address(task$backend), address(Tasks$env$bh$backend)))

  data = data.table(x = 1:30, y = factor(sample(letters[1:2], 30, replace = TRUE)))
  task = TaskClassif$new(data, target = "y", id = "testthat-example")
  Tasks$add(task$clone(deep = TRUE), overwrite = TRUE)
  on.exit(Tasks$remove("testthat-example"))

  rtask = Tasks$get("testthat-example")
  expect_true(!identical(address(task), address(rtask)))
  expect_true(!identical(address(task$backend), address(rtask$backend)))

  task$backend$subsample(ratio = 0.5)
  expect_identical(rtask$backend$nrow, 30L)

  task = Tasks$get("sonar")
  task$backend$subsample(ratio = 0.5)
  expect_identical(Tasks$get("sonar")$backend$nrow, 208L)
})
