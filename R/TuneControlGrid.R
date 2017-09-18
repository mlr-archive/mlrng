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
