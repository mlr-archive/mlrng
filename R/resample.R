ResampleResult = R6Class("ResampleResult",
  cloneable = FALSE,
  public = list(
    data = NULL,
    measure = NULL,
    initialize = function(result, measure) {
      self$data = result
      self$measure = measure
    },
    performance = function(id) {
      rbindlist(lapply(rr$data, function(x) as.list(x$performance)))
    }
  ),
  active = list(
    aggr = function() {
      mean(unlist(lapply(self$data, "[[", "performance")))
    }
  )
)

#' @export
as.data.table.ResampleResult = function(x, keep.rownames = FALSE, ...) {
  rbindlist(lapply(x$data, function(x) {
    c(list(
      predicted = list(x$predicted),
      model = list(x$model)
    ), unlist(x$performance))
  }), idcol = "iter")
}

#' @export
resample = function(task, learner, resampling, measures) {
  task = getTask(task)
  learner = getLearner(learner)
  resampling = getResampling(resampling, task)
  measures = getMeasures(measures)

  pm.level = "mlrng.resample"
  parallelLibrary(packages = "mlrng", master = FALSE, level = pm.level)
  result = parallelMap(
    resampleIteration,
    i = seq_along(resampling),
    more.args = list(task = task, learner = learner, resampling = resampling, measures = measures),
    level = pm.level
  )

  ResampleResult$new(result, measures)
}

resampleIteration = function(task, learner, resampling, measures, i) {
  train = resampling[[i]]
  test = which(!train)
  gmessage("[Resample]: task={task$id} | learner={learner$id} | resampling={resampling$id}: {i}/{length(resampling)}")
  model = trainWithHooks(task = task, learner = learner, subset = which(train))
  truth = task[[task$target]][test]
  predicted = predictWithHooks(model, task, learner, subset = test)
  performance = lapply(measures, function(x) x$fun(truth, predicted$predicted))
  list(
    model = model,
    predicted = predicted$predicted,
    performance = setNames(performance, ids(measures))
  )
}
