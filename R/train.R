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
#' @return \code{\link{MlrModel}}.
#' @export
train = function(task, learner, subset = NULL) {
  assertR6(task, "Task")
  assertR6(learner, "Learner")

  train = translateSubset(task, subset)
  model = NULL
  raw.log = evaluate::evaluate("model = trainWorker(task, learner, train)",
    include_timing = TRUE, new_device = FALSE)

  train.log = TrainLog$new(raw.log)

  if (is.null(model) || train.log$n.errors > 0)
    gstop("Training {learner$id} on {task$id} failed with {train.log$n.errors} errors!")
    #FIXME: add Dummy learner and option to continue on error

  gVerboseMessage("Trained {learner$id} on {task$id} with {train.log$n.errors} errors, {train.log$n.warnings} warnings and {train.log$n.messages} messages.")

  MlrModel$new(task, learner, model, train, train.log)
}

trainWorker = function(task, learner, subset) {
  assertInteger(subset, lower = 1L, upper = task$nrow, any.missing = FALSE)
  pars = c(list(task = task, subset = subset), learner$par.vals)
  do.call(learner$train, pars)
}
