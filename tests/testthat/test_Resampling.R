context("Resampling")

no_intersects = function(r) {
  all(lengths(Map(intersect, r$instance$train, r$instance$test)) == 0L)
}

ind_lengths = function(r, w) {
  BBmisc::viapply(seq_len(r$iters), function(i) length(r$instance[[w]][[i]]))
}

test_that("resampling cv", {
  task = mlr.tasks$get("iris")
  r = mlr.resamplings$get("cv")
  r$instantiate(task)
  expect_list(r$pars, len = 0L)
  expect_identical(r$iters, 10L)
  expect_resampling(r, task)
  expect_true(all(ind_lengths(r, "train") == 135L))
  expect_true(all(ind_lengths(r, "test") == 15L))
  expect_true(no_intersects(r))
})

test_that("resampling holdout", {
  task = mlr.tasks$get("sonar")
  r = mlr.resamplings$get("holdout")$instantiate(task)
  expect_identical(r$iters, 1L)
  expect_equal(r$pars, list(ratio = 2/3))
  expect_resampling(r, task)
  expect_true(all(ind_lengths(r, "train") == 138L))
  expect_true(all(ind_lengths(r, "test") == 70L))
  expect_true(no_intersects(r))
})

test_that("resampling subsample", {
  task = mlr.tasks$get("pima")
  r = mlr.resamplings$get("subsampling")$instantiate(task)
  expect_identical(r$iters, 30L)
  expect_equal(r$pars, list(ratio = 2/3))
  expect_resampling(r, task)
  expect_true(all(ind_lengths(r, "train") == 512L))
  expect_true(all(ind_lengths(r, "test") == 256L))
  expect_true(no_intersects(r))
})
