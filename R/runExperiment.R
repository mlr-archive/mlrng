runExperiment = function(task, learner, resampling, resampling.iter, measures, store.model = TRUE) {
  # FIXME: if we want to do zero checks here, next 2 lines are out!
  if (is.null(resampling$instance))
    stop("Resampling has not been instantiated yet")

  ginfo("[Experiment]: task={task$id} | learner={learner$id} | resampling={resampling$id}: {resampling.iter}/{resampling$iters}")

  train.ids = resampling$train.set(resampling.iter)
  test.ids = resampling$test.set(resampling.iter)

  result = trainWorker(task = task, learner = learner, row.ids = train.ids)

  result = predictWorker(result, newdata = getTaskData(task, subset = test.ids, type = "test", props = learner$properties), row.ids = test.ids)

  result = performance(result, measures = measures)
  if (!store.model)
    result$data[, "rmodel" := list(list(NULL))][]
  return(result)
}
