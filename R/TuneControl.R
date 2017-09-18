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
