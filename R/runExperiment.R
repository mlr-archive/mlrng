runExperiment = function(task, learner, resampling, resampling.iter, measures, store.model = TRUE) {
  if (is.null(resampling$instance))
    stop("Resampling has not been instantiated yet")
  split = resampling$split(resampling.iter)
  train = split$train
  test = split$test

  gmessage("[Experiment]: task={task$id} | learner={learner$id} | resampling={resampling$id}: {resampling.iter}/{resampling$iters}")

  model = train(task = task, learner = learner, subset = train)
  pred = predict(model, task = task, subset = test)
  perf = performance(pred, measures = measures)

  data.table(
    task = list(task),
    learner = list(learner),
    resampling = list(resampling),
    split = list(split),
    model = if (store.model) list(model) else list(NULL),
    pred = list(pred),
    perf = list(perf)
  )
}
