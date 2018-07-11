#' @title Resample a Learner on a Task
#'
#' @description
#' Runs a resampling (possibly in parallel).
#'
#' @param task [[Task()]]\cr
#'   Object of type [Task()].
#' @param learner [[Learner()]]\cr
#'   Object of type [Learner()].
#' @param resampling [[Resampling()]]\cr
#'   Object of type [Resampling()].
#' @param measures [`list` of [Measure()]]\cr
#'   List of objects of type [Measure()].
#' @return [ResampleResult()].
#' @export
resample = function(task, learner, resampling, measures) {
  assertTask(task)
  assertLearner(learner, for.task = task)
  assertResampling(resampling, for.task = task)
  assertMeasures(measures, for.task = task, for.learner = learner)

  # FIXME: why is store.model not an option here?

  if (is.null(resampling$instance))
    resampling = resampling$clone()$instantiate(task)

  pm.level = "mlrng.resample"
  parallelLibrary(packages = "mlrng", master = FALSE, level = pm.level)
  results = parallelMap(
    runExperiment,
    resampling.iter = seq_len(resampling$iters),
    more.args = list(task = task, learner = learner, measures = measures, resampling = resampling),
    level = pm.level
  )

  ResampleResult$new(results)
}
