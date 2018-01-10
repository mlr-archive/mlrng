context("TaskClassif")

test_that("Basic ops on iris task", {
  dd = as.data.table(iris)
  task = mlr.tasks$get("iris")
  expect_classiftask(task)
  expect_equal(task$target, "Species")

  expect_set_equal(task$classes, levels(iris$Species))
  expect_identical(task$nclasses, nlevels(iris$Species))

  f = task$formula
  expect_class(f, "formula")
  expect_set_equal(attr(terms(f), "term.labels"), setdiff(names(dd), "Species"))
})

test_that("$classes and $nclasses only consider active rows", {
  task = test.tasks$get("clm.num")
  address(task)
  task2 = test.tasks$get("clm.num")
  address(task2)
  task$rows[3:50, role := "ignore"]
  expect_identical(task$classes, "setosa")
  expect_identical(task$nclasses, 1L)
})
