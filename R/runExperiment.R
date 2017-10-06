runExperiment = function(task, learner, resampling, resampling.iter, measures, store.model = TRUE) {
  # FIXME: if we want to do zero checks here, next 2 lines are out!
  if (is.null(resampling$instance))
    stop("Resampling has not been instantiated yet")

  gmessage("[Experiment]: task={task$id} | learner={learner$id} | resampling={resampling$id}: {resampling.iter}/{resampling$iters}")

  #FIXME: check later whether we want to construct this pipeline slightly better
  result = train(task = task, learner = learner, subset = resampling$train.set(task, resampling.iter))
  result = predict(result, subset = resampling$test.set(task, resampling.iter))
  result = performance(result, measures = measures)
  if (!store.model)
    result$data[, "rmodel" := list(list(NULL))][]
  return(result)
}
