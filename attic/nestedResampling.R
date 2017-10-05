getNestedResampling = function(outer, inner) {
  assertClass(outer, "Resampling")
  assertClass(inner, "Resampling")

  r = Resampling$new(
    id = "nested resampling",
    instantiate = function(x) {
      self$pars$outer$instantiate(task)
      no = self$pars$outer$iters
      ni = self$pars$inner$iters
      tmp = vector("list", no * ni)
      self$instance = list(train = tmp, test = tmp)

      for (i in seq_len(no)) {
        train = self$pars$outer$train.set(i)
        self$pars$inner$instantiate(length(train))
        for (j in seq_len(ni)) {
          ij = (i-1L) * ni + j
          # ind = as.bitwhich(train)
          self$instance$train[[ij]] = intersect(train, self$pars$inner$train(j))
          self$instance$test[[ij]] = replace(train, ind, self$pars$inner$instance$test[[j]])
        }
      }
    },
    iters = length(inner) * length(outer),
    pars = list(inner = inner, outer = outer)
  )
}


test_that("nested resampling", {
  skip("Not finished yet")
  task = mlr.tasks$get("iris")
  outer = mlr.resamplings$get("cv")
  inner = mlr.resamplings$get("cv")
  inner$iters = 3

  r = getNestedResampling(outer, inner)
  expect_identical(r$iters, 30L)
  expect_resampling(r, task)

  expect_equal(BBmisc::viapply(r$instance$train, sum), rep(90L, r$iters)) # 150 * 9/10 * 2/3
  expect_equal(BBmisc::viapply(r$instance$test, sum), rep(45L, r$iters)) # 150 * 9/10 * 1/3
})
