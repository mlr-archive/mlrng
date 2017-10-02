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
  raw.log = evaluate::evaluate("model = trainWorker(task, learner, train)", include_timing = TRUE)
  if (is.null(model) || any(vlapply(raw.log, is.error)))
    stop("Model fit failed")

  MlrModel$new(task, learner, model, train, raw.log)
}

trainWorker = function(task, learner, subset) {
  assertInteger(subset, lower = 1L, upper = task$nrow, any.missing = FALSE)
  learner$train(task, subset = subset, data = task$data(subset))
}
