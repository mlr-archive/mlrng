#' @title Resample a Learner on a Task
#'
#' @description
#' Runs a resampling (possibly in parallel).
#'
#' @param task [\code{\link{Task}}]\cr
#'   Object of type \code{\link{Task}}.
#' @param learner [\code{\link{Learner}}]\cr
#'   Object of type \code{\link{Learner}}.
#' @param resampling [\code{\link{Resampling}}]\cr
#'   Object of type \code{\link{Resampling}}.
#' @param measures [\code{list} of \code{\link{Measure}}]\cr
#'   List of objects of type \code{\link{Measure}}.
#' @return \code{\link{ResampleResult}}.
#' @export
resample = function(task, learner, resampling, measures) {
  assertClass(task, "Task")
  assertClass(learner, "Learner")
  assertClass(resampling, "Resampling")
  assertList(measures, "Measure")

  if (is.null(resampling$instance))
    resampling = resampling$clone()$instantiate(task)

  pm.level = "mlrng.resample"
  parallelLibrary(packages = "mlrng", master = FALSE, level = pm.level)
  result = parallelMap(
    resampleIteration,
    i = seq_len(resampling$iters),
    more.args = list(task = task, learner = learner, measures = measures, resampling = resampling),
    level = pm.level
  )

  ResampleResult$new(result, measures)
}

resampleIteration = function(i, task, learner, resampling, measures, store.model = TRUE) {
  split = resampling$split(i)
  test = split$test

  gmessage("[Resample]: task={task$id} | learner={learner$id} | resampling={resampling$id}: {i}/{length(resampling)}")
  model = trainWithHooks(task = task, learner = learner, subset = split$train)
  truth = task$backend$get(ids = task$backend$active.rows[test], task$target)
  predicted = predictWithHooks(model, task, learner, subset = test)
  performance = lapply(measures, function(x) x$fun(truth, predicted$response))
  list(
    task = task,
    model = model,
    split = split,
    model = if (store.model) model else NULL,
    predicted = predicted$response,
    performance = setNames(performance, ids(measures))
  )
}
