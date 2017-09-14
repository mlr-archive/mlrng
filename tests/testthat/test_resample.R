context("resample")

test_that("Basic resampling", {
  learner = getLearner("classif.rpart", mtry = 2)
  learner = downsampleWrapper(learner, ratio = 0.1)
  rr = resample("iris", learner, "cv", "mmce")
  expect_class(rr, "ResampleResult")
  expect_list(rr$data, len = 10, names = "unnamed")
  expect_data_table(as.data.table(rr), nrow = 10, ncol = 4)
})

test_that("resampling cv", {
  task = getTask("iris")
  r = getResampling("cv")
  expect_identical(r$iters, 10L)
  expect_resampling(r, task)
  expect_equal(BBmisc::viapply(r$instance, function(x) length(x$train)), rep(135, r$iters))
  expect_equal(BBmisc::viapply(r$instance, function(x) length(x$test)), rep(15, r$iters))
  expect_true(all(BBmisc::vlapply(Map(xor, r$instance$train, r$instance$test), all)))
})

test_that("resampling holdout", {
  task = getTask("sonar")
  r = getResampling("holdout")
  expect_identical(r$iters, 1L)
  expect_equal(r$pars, list(ratio = 2/3))
  expect_resampling(r, task)
  expect_equal(BBmisc::viapply(r$instance, function(x) length(x$train)), 138L)
  expect_equal(BBmisc::viapply(r$instance, function(x) length(x$test)), 70L)
  expect_true(all(BBmisc::vlapply(Map(xor, r$instance$train, r$instance$test), all)))
})

test_that("resampling subsample", {
  task = getTask("pima")
  r = getResampling("subsampling")
  expect_identical(r$iters, 30L)
  expect_equal(r$pars, list(ratio = 2/3))
  expect_resampling(r, task)
  expect_equal(BBmisc::viapply(r$instance, function(x) length(x$train)), rep(512L, r$iters))
  expect_equal(BBmisc::viapply(r$instance, function(x) length(x$test)), rep(256L, r$iters))
  expect_true(all(BBmisc::vlapply(Map(xor, r$instance$train, r$instance$test), all)))
})

test_that("nested resampling", {
  skip("Not finished yet")
  task = getTask("iris")
  outer = getResampling("cv")
  inner = getResampling("cv")
  inner$iters = 3

  r = getNestedResampling(outer, inner)
  expect_identical(r$iters, 30L)
  expect_resampling(r, task)

  expect_equal(BBmisc::viapply(r$instance$train, sum), rep(90L, r$iters)) # 150 * 9/10 * 2/3
  expect_equal(BBmisc::viapply(r$instance$test, sum), rep(45L, r$iters)) # 150 * 9/10 * 1/3
})
