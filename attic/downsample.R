downsample = function(task, ratio) {
  assertR6(task, "Task")
  assertNumber(ratio, lower = 0, upper = 1)

  n = task$nrow
  status = NULL
  task$data[status == "active"][sample(n, n * (1 - ratio)), status := "inactive"]
}

downsampleWrapper = function(lrn, ratio = 0.1) {
  lrn = getLearner(lrn)
  assertNumber(ratio, lower = 0, upper = 1)

  lrn$addHook("downsample",
    hooks = list(
      pre.train = function(task, learner, subset, pars) {
        n = task$nrow
        list(subset = sample(seq_len(n), ceiling(n * pars$ratio)))
      },
      pars = list(ratio = ratio)
    )
  )

  return(lrn)
}


# test_that("Basic resampling", {
#   learner = mlr.learners$get("classif.rpart", mtry = 2)
#   learner = downsampleWrapper(learner, ratio = 0.1)
#   rr = resample("iris", learner, "cv", "mmce")
#   expect_class(rr, "ResampleResult")
#   expect_list(rr$data, len = 10, names = "unnamed")
#   expect_data_table(as.data.table(rr), nrow = 10, ncol = 4)
# })
