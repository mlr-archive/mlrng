context("Resampling")

test_that("resampling cv", {
  task = mlr.tasks$get("iris")
  r = mlr.resamplings$get("cv")
  r$instantiate(task)
  expect_identical(r$iters, 10L)
  expect_resampling(r, task)
  expect_equal(BBmisc::viapply(r$instance, function(x) length(x$train.set)), rep(135, r$iters))
  expect_equal(BBmisc::viapply(r$instance, function(x) length(x$test.set)), rep(15, r$iters))
  expect_true(all(BBmisc::vlapply(Map(xor, r$instance$train.set, r$instance$test.set), all)))
})

test_that("resampling holdout", {
  task = mlr.tasks$get("sonar")
  r = mlr.resamplings$get("holdout")$instantiate(task)
  expect_identical(r$iters, 1L)
  expect_equal(r$pars, list(ratio = 2/3))
  expect_resampling(r, task)
  expect_equal(BBmisc::viapply(r$instance, function(x) length(x$train.set)), 138L)
  expect_equal(BBmisc::viapply(r$instance, function(x) length(x$test.set)), 70L)
  expect_true(all(BBmisc::vlapply(Map(xor, r$instance$train.set, r$instance$test.set), all)))
})

test_that("resampling subsample", {
  task = mlr.tasks$get("pima")
  r = mlr.resamplings$get("subsampling")$instantiate(task)
  expect_identical(r$iters, 30L)
  expect_equal(r$pars, list(ratio = 2/3))
  expect_resampling(r, task)
  expect_equal(BBmisc::viapply(r$instance, function(x) length(x$train.set)), rep(512L, r$iters))
  expect_equal(BBmisc::viapply(r$instance, function(x) length(x$test.set)), rep(256L, r$iters))
  expect_true(all(BBmisc::vlapply(Map(xor, r$instance$train.set, r$instance$test.set), all)))
})
