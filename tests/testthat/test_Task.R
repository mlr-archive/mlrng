context("Task")


test_that("Task Construction", {
  dbe = DataBackendDataTable$new(data = iris)
  task = Task$new(id = "foo", backend = dbe)
  expect_task(task)
})

test_that("Example Tasks", {
  for (id in Tasks$ids) {
    task = Tasks$get(id)
    expect_data_table(task$backend$get(), types = c("numeric", "factor"))
  }
})


