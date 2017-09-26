# if (FALSE) {
#   library(ParamHelpers)
#   task = Tasks$get("iris")
#   learner = Learners$get("classif.rpart")
#   resampling = Resamplings$get("cv")
#   resampling$instantiate(task)
#   inner = Resamplings$get("holdout")
#   measures = Measures$get("mmce")
#   par.set = makeParamSet(
#     makeIntegerParam("mtry", lower = 1, upper = 10)
#   )
#   ctrl = TuneControlGrid$new(par.set, 20, inner)

#   ctrl$run(task ,learner, 1:50, mmce)
#   self = list(
#     par.set = par.set,
#     budget = 5,
#     resampling = inner
#   )
# }
tune = function(task, learner, resampling, measures, ctrl) {
  assertR6(task, "Task")
  assertR6(learner, "Learner")
  assertR6(resampling, "Resampling")
  assertList(measures, "Measure")
  assertR6(ctrl, "TuneControl")

  # if (is.null(resampling$instance))
  #   resampling$instantiate(task)

  # pm.level = "mlrng.tune"
}
