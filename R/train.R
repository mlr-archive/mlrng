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
  model = trainWorker(task, learner, split$train)
  WrappedModel$new(task, learner, model, split)
}

trainWorker = function(task, learner, subset) {
  assertInteger(subset, lower = 1L, upper = task$backend$nrow, any.missing = FALSE)
  learner$train(task, subset = subset,
    data = task$backend$get(i = subset))
}
