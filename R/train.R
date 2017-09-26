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
  assertR6(task, "Task")
  assertR6(learner, "Learner")

  subset = asSubset(task, subset)
  split = Split$new(train = subset)
  model = trainWorker(task, learner, split)
  WrappedModel$new(task, learner, model, split)
}

trainWorker = function(task, learner, split) {
  learner$train(task, subset = split$train)
}
