context("ClassifTask")

test_that("Basic ops on iris task", {
  dd = as.data.table(iris)
  task = Tasks$get("iris")
  for (task in list(task, asDplyrTask(task))) {
    expect_class(task, "Task")
    expect_equal(task$target, "Species")
    expect_identical(task$nrow, 150L)
    expect_identical(task$ncol, 5L)

    expect_equal(task[1:10], dd[1:10])
    expect_equal(task[[task$target]], dd$Species)
    f = task$formula
    expect_class(f, "formula")
    expect_set_equal(attr(terms(f), "term.labels"), setdiff(names(dd), "Species"))
  }
})
