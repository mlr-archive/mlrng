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

TuneControl = R6Class("TuneControl",
  cloneable = FALSE,
  public = list(
    budget = NA_integer_,
    par.set = NULL,
    resampling = NULL,
    opt.path = NULL,
    initialize = function(par.set, budget, resampling) {
      self$par.set = assertClass(par.set, "ParamSet")
      self$budget = assertCount(budget)
      self$resampling = assertClass(getResampling(resampling), "Resampling")
    }
  )
)

TuneControlGrid = R6Class("TuneControlGrid",
  cloneable = FALSE,
  inherit = TuneControl,
  public = list(
    run = function(task, learner, resampling, measures) {
      design = setDT(ParamHelpers::generateGridDesign(self$par.set, self$budget))
      jobs = CJ(i.design = seq_row(design), i.outer = seq_len(resampling$iters), i.inner = seq_len(self$resampling$iters))

      res = parallelMap(function(i) {
        row = jobs[i]
        pars = as.list(design[row$i.design])
        learner = learner$clone()
        learner$par.vals = pars
        subset = resampling[[row$i.outer]]
        subtask = task$clone(deep = TRUE)
        subtask$backend$slice(subset)
        inner = self$resampling$clone()
        inner$instantiate(subtask) # FIXME: aawww fuck, this does not work :(
        gmessage("[Tune]: Trying Hyperpars: {listToShortString(pars)}")
        resampleIteration(subtask, learner, inner, measures, row$i.inner)
      }, i = seq_row(jobs))
    }
  )
)

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

