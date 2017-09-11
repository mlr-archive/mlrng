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
  expect_equal(viapply(r$instance$train, sum), rep(135, r$iters))
  expect_equal(viapply(r$instance$test, sum), rep(15, r$iters))
})

test_that("resampling holdout", {
  task = getTask("sonar")
  r = getResampling("holdout")
  expect_identical(r$iters, 1L)
  expect_equal(r$pars, list(ratio = 2/3))
  expect_resampling(r, task)
  expect_equal(viapply(r$instance$train, sum), 138L)
  expect_equal(viapply(r$instance$test, sum), 70L)
})

test_that("resampling subsample", {
  task = getTask("pima")
  r = getResampling("subsampling")
  expect_identical(r$iters, 30L)
  expect_equal(r$pars, list(ratio = 2/3))
  expect_resampling(r, task)
  expect_equal(viapply(r$instance$train, sum), rep(512L, r$iters))
  expect_equal(viapply(r$instance$test, sum), rep(256L, r$iters))
})
