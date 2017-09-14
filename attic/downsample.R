downsample = function(task, ratio) {
  task = getTask(task)
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
