BenchmarkResult = R6Class("BenchmarkResult",
  cloneable = FALSE,
  public = list(
    data = NULL,
    initialize = function(tasks, learners, resamplings) {
      self$data = rbindlist(Map(
        function(task.id, learner.id, resampling.id) {
          CJ(
            task = task.id,
            learner = learner.id,
            resampling = resampling.id,
            iter = seq_along(resamplings[[resampling.id]])
          )
        }, task.id = names(tasks), learner.id = names(learners), resampling.id = names(resamplings)))
    },
    store = function(res) {
      self$data = cbind(self$data, rbindlist(lapply(res, function(x) {
        c(list(
          split = list(x$split),
          model = list(x$model),
          predicted = list(x$predicted)),
        unlist(x$performance))
      })))
    }
  )
)


#' @export
benchmark = function(tasks, learners, resamplings, measures) {
  assertList(tasks, "Task", min.len = 1L)
  assertList(learners, "Learner", min.len = 1L)
  assertList(resamplings, "Resampling", min.len = 1L)
  assertList(measures, "Measure", min.len = 1L)
  names(tasks) = assertNames(ids(tasks), "unique")
  names(learners) = assertNames(ids(learners), "unique")
  names(resamplings) = assertNames(ids(resamplings), "unique")
  names(measures) = assertNames(ids(measures), "unique")

  # instantiate resamplings for each task
  rr = setNames(vector("list", length(tasks)), names(tasks))
  for (tn in names(tasks)) {
    rr[[tn]] = setNames(vector("list", length(resamplings)), names(resamplings))
    for (rn in names(resamplings)) {
      rr[[tn]][[rn]] = resamplings[[rn]]$clone()
      rr[[tn]][[rn]]$instantiate(tasks[[tn]])
    }
  }
  rr = unlist(rr, recursive = FALSE)


  bmr = BenchmarkResult$new(tasks, learners, rr)
  pm.level = "mlrng.resample"
  parallelLibrary(packages = "mlrng", master = FALSE, level = pm.level)
  result = parallelMap(
    fun = resampleIteration,
    task = tasks[bmr$data$task],
    learner = learners[bmr$data$learner],
    resampling = rr[bmr$data$resampling],
    i = bmr$data$iter,
    more.args = list(measures = measures, store.model = FALSE),
    level = pm.level
  )
  bmr$store(result)
  bmr
}

#' @export
as.data.table.BenchmarkResult = function(x, keep.rownames = FALSE, ...) {
  copy(x$data)
}
