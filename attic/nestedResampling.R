getNestedResampling = function(outer, inner) {
  assertClass(outer, "Resampling")
  assertClass(inner, "Resampling")

  Resampling$new(
    id = "nested resampling",
    description = sprintf("nested resampling: [%s]x[%s]", outer$id, inner$id),
    instantiate = function(x) {
      if (inherits(x, "Task"))
        x = x$nrow
      assertCount(x)

      self$pars$outer$instantiate(x)
      no = length(self$pars$outer)
      ni = length(self$pars$inner)
      tmp = vector("list", no * ni)
      self$instance = list(train = tmp, test = tmp)

      for (i in seq_len(no)) {
        train = self$pars$outer$instance$train[[i]]
        self$pars$inner$instantiate(sum(train))
        for (j in seq_len(ni)) {
          ij = (i-1L) * ni + j
          ind = as.bitwhich(train)
          self$instance$train[[ij]] = replace(train, ind, self$pars$inner$instance$train[[j]])
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
