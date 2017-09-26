#' @title Benchmark Multiple Learners on Multiple Tasks
#'
#' @description
#' Runs a benchmark (possibly in parallel).
#'
#' @param tasks [\code{list} of \code{\link{Task}}]\cr
#'   List of objects of type \code{\link{Task}}.
#' @param learners [\code{list} of \code{\link{Learner}}]\cr
#'   List of objects of type \code{\link{Learner}}.
#' @param resamplings [\code{list} of \code{\link{Resampling}}]\cr
#'   List of objects of type \code{\link{Resampling}}.
#' @param measures [\code{list} of \code{\link{Measure}}]\cr
#'   List of objects of type \code{\link{Measure}}.
#' @return \code{\link{BenchmarkResult}}.
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
      rr[[tn]][[rn]] = resamplings[[rn]]$clone()$instantiate(tasks[[tn]])
    }
  }
  rr = unlist(rr, recursive = FALSE)

  bmr = BenchmarkResult$new(tasks, learners, resamplings)
  pm.level = "mlrng.resample"
  parallelLibrary(packages = "mlrng", master = FALSE, level = pm.level)
  result = parallelMap(
    fun = resampleIteration,
    task = tasks[bmr$data$task],
    learner = learners[bmr$data$learner],
    resampling = rr[sprintf("%s.%s", bmr$data$task, bmr$data$resampling)],
    i = bmr$data$iter,
    more.args = list(measures = measures, store.model = FALSE),
    level = pm.level
  )
  bmr$store(result)
  bmr
}
