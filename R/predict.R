predictWithHooks = function(model, task, learner, subset) {
  assertInteger(subset, any.missing = FALSE, null.ok = TRUE) # FIXME: IFDEBUG

  # ee = new.env(parent = emptyenv())
  # ee$model = model
  # ee$task = task
  # ee$learner = learner
  # ee$subset = subset %??% seq_len(task$nrow)
  # runHook(ee, ee$learner$hooks, "pre.predict")
  # ee$predicted = do.call(ee$learner$predict, c(list(model = ee$model, task = ee$task, subset = ee$subset), ee$learner$par.vals))
  # runHook(ee, ee$learner$hooks, "post.predict")

  subset = subset %??% seq_len(task$nrow)
  predicted = do.call(learner$predict, c(list(model = model, task = task, subset = subset), learner$par.vals))
  Prediction$new(task = task, subset = subset, response = predicted)
}

#' @export
predict.WrappedModel = function(object, task, subset = NULL, ...) {
  assertIntegerish(subset, null.ok = TRUE, any.missing = FALSE)
  predictWithHooks(model = object$model, task = task, learner = object$learner, subset = subset)
}
