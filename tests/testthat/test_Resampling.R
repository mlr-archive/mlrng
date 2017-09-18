context("resample")

test_that("resampling cv", {
  task = Tasks$get("iris")
  r = Resamplings$get("cv")
  expect_identical(r$iters, 10L)
  expect_resampling(r, task)

  expect_is(r[[1]], "Split")
  expect_equal(BBmisc::viapply(r$instance, function(x) length(x$train)), rep(135, r$iters))
  expect_equal(BBmisc::viapply(r$instance, function(x) length(x$test)), rep(15, r$iters))
  expect_true(all(BBmisc::vlapply(Map(xor, r$instance$train, r$instance$test), all)))
})

test_that("resampling holdout", {
  task = Tasks$get("sonar")
  r = Resamplings$get("holdout")
  expect_identical(r$iters, 1L)
  expect_equal(r$pars, list(ratio = 2/3))
  expect_resampling(r, task)
  expect_equal(BBmisc::viapply(r$instance, function(x) length(x$train)), 138L)
  expect_equal(BBmisc::viapply(r$instance, function(x) length(x$test)), 70L)
  expect_true(all(BBmisc::vlapply(Map(xor, r$instance$train, r$instance$test), all)))
})

test_that("resampling subsample", {
  task = Tasks$get("pima")
  r = Resamplings$get("subsampling")
  expect_identical(r$iters, 30L)
  expect_equal(r$pars, list(ratio = 2/3))
  expect_resampling(r, task)
  expect_equal(BBmisc::viapply(r$instance, function(x) length(x$train)), rep(512L, r$iters))
  expect_equal(BBmisc::viapply(r$instance, function(x) length(x$test)), rep(256L, r$iters))
  expect_true(all(BBmisc::vlapply(Map(xor, r$instance$train, r$instance$test), all)))
})

test_that("nested resampling", {
  skip("Not finished yet")
  task = Tasks$get("iris")
  outer = Resamplings$get("cv")
  inner = Resamplings$get("cv")
  inner$iters = 3

  r = getNestedResampling(outer, inner)
  expect_identical(r$iters, 30L)
  expect_resampling(r, task)

  expect_equal(BBmisc::viapply(r$instance$train, sum), rep(90L, r$iters)) # 150 * 9/10 * 2/3
  expect_equal(BBmisc::viapply(r$instance$test, sum), rep(45L, r$iters)) # 150 * 9/10 * 1/3
})
