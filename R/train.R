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
  learner$train(task, subset = subset %??% seq_len(task$backend$nrow))
}

#' @title Train a Learner on a Task
#'
#' @description
#' Fits a model on a subset of the task data.
#'
#' @param task [\code{\link{Task}}]\cr
#'   Object of type \code{\link{Task}}.
#' @param learner [\code{\link{Learner}}]\cr
#'   Object of type \code{\link{Learner}}.
#' @param subset [\code{integer} | \code{logical}]\cr
#'   Subset of \code{task} to train the data on.
#' @return \code{\link{WrappedModel}}.
#' @export
train = function(task, learner, subset = NULL) {
  resampling = makePseudoHoldout(task, asSubset(task, subset))
  mod = trainWithHooks(task = task, learner = learner, subset = subset)
  WrappedModel$new(task, learner, mod, resampling)
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
