trainWithHooks = function(task, learner, subset = NULL) {
  # # FIXME: the env can be created as-needed in runHook.
  # #        we already have reference semantic?
  # ee = new.env(parent = emptyenv())
  # ee$task = task
  # ee$learner = learner
  # ee$subset = subset %??% seq_len(task$nrow)
  # runHook(ee, learner$hooks, "pre.train")
  # ee$model = ee$learner$train(ee$task, subset = ee$subset)
  # runHook(ee, learner$hooks, "post.train")
  # ee$model
  learner$train(task, subset = subset %??% seq_len(task$nrow))
}

#' @export
train = function(task, learner, subset = NULL) {
  resampling = makePseudoHoldout(task, asSubset(task, subset))
  WrappedModel$new(task, learner, trainWithHooks(task = task, learner = learner, subset = subset), resampling)
}

makePseudoHoldout = function(task, subset = NULL) {
  ph = Resampling$new(
    id = "CustomHoldout",
    description = "Custom holdout-like resampling",
    iters = 1L,
    instantiate = NULL
  )
  ph$set(train = list(subset))
}
