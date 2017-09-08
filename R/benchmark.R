BenchmarkResult = R6Class("BenchmarkResult",
  cloneable = FALSE,
  public = list(
    data = NULL,
    initialize = function(task.ids, learner.ids, resamplings) {
      self$data = rbindlist(Map(
        function(task.id, resampling) {
          CJ(task = task.id, learner = learner.ids, iter = seq_along(resampling))
        }, task.id = task.ids, resampling = resamplings))
    },
    store = function(res) {
      self$data = cbind(self$data, rbindlist(lapply(res, function(x) {
        c(list(
          model = list(x$model),
          predicted = list(x$predicted)),
        unlist(x$performance))
      })))
    }
  )
)

benchmark = function(tasks, learners, resamplings, measures) {
  tasks = getTasks(tasks)
  learners = getLearners(learners)
  resamplings = getResamplings(resamplings, tasks = tasks)
  names(resamplings) = names(tasks)
  measures = getMeasures(measures)

  bmr = BenchmarkResult$new(names(tasks), names(learners), resamplings)
  pm.level = "mlrng.resample"
  parallelLibrary(packages = "mlrng", master = FALSE, level = pm.level)
  result = parallelMap(
    fun = resampleIteration,
    task = tasks[bmr$data$task],
    learner = learners[bmr$data$learner],
    resampling = resamplings[bmr$data$task],
    i = bmr$data$iter,
    more.args = list(measures = measures, store.model = FALSE),
    level = pm.level
  )
  bmr$store(result)
  bmr
}
