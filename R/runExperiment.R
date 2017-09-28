runExperiment = function(task, learner, resampling, resampling.iter, measures) {
  if (is.null(resampling$instance))
    stop("Resampling has not been instantiated yet")
  split = resampling$split(resampling.iter)
  train = split$train
  test = split$test

  gmessage("[Experiment]: task={task$id} | learner={learner$id} | resampling={resampling$id}: {resampling.iter}/{resampling$iters}")

  model = trainWorker(task = task, learner = learner, subset = train)
  response = predictWorker(model = model, task = task, learner = learner, subset = test)
  truth = task$backend$get(ids = task$backend$active.rows[test], task$target)[[1L]]
  performance = lapply(measures, function(x) x$fun(truth, response))

  data.table(
    task = list(task),
    learner = list(learner),
    resampling = list(resampling),
    split = list(split),
    model = list(model),
    response = list(response),
    truth = list(truth),
    performance = list(performance)
  )
}
