if (FALSE) {
  library(ParamHelpers)
  task = getTask("iris")
  learner = getLearner("classif.rpart")
  resampling = getResampling("cv")
  resampling$instantiate(task)
  inner = getResampling("holdout")
  measures = getMeasures("mmce")
  par.set = makeParamSet(
    makeIntegerParam("mtry", lower = 1, upper = 10)
  )
  ctrl = TuneControlGrid$new(par.set, 20, inner)

  ctrl$run(task ,learner, 1:50, mmce)
  self = list(
    par.set = par.set,
    budget = 5,
    resampling = inner
  )
}

tune = function(task, learner, resampling, measures, ctrl) {
  task = getTask(task)
  learner = getLearner(learner)
  assertClass(ctrl$par.set, "ParamSet")
  resampling = getResampling(resampling)
  measures = getMeasures(measures)

  if (is.null(resampling$instance))
    resampling$instantiate(task)

  pm.level = "mlrng.tune"
}

