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
#' @return \code{\link{TrainResult}}.
#' @export
train = function(task, learner, subset = NULL) {
  assertTask(task)
  assertLearner(learner, for.task = task)
  assertIndexSet(subset, for.task = task)

  subset = translateSubset(task, subset)
  wrapped.model = NULL
  encapsulation = getOption("mlrng.train.encapsulation", 1L)

  start.time = proc.time()[3L]

  if (encapsulation == 0L) {
    wrapped.model = trainWorker(task, learner, subset)
    raw.log = list()
  } else {
    eval.string = getTrainEvalString(encapsulation)
    raw.log = evaluate::evaluate(eval.string, new_device = FALSE)
  }

  train.time = proc.time()[3L] - start.time

  train.log = TrainLog$new(raw.log, train.time)
  train.success = !is.null(wrapped.model) && train.log$n.errors == 0L

  if (train.success) {
    ginfo("Trained {learner$id} on {task$id} with {train.log$n.errors} errors, {train.log$n.warnings} warnings and {train.log$n.messages} messages.")
  } else {
   if (getOption("mlrng.continue.on.learner.error", FALSE)) {
      wrapped.model = trainFailureModel(task, subset)
      ginfo("Training {learner$id} on {task$id} failed, fallback to dummy model.")
    } else {
      gstop("Training {learner$id} on {task$id} failed with {train.log$errors[[1]]$message}.")
    }
  }

  TrainResult$new(task, learner, wrapped.model, subset, train.log)
}

trainWorker = function(task, learner, subset) {
  assertInteger(subset, lower = 1L, upper = task$nrow, any.missing = FALSE)
  pars = c(list(task = task, subset = subset), learner$par.vals)
  do.call(learner$train, pars)
}

trainFailureModel = function(task, subset) {
  fallback.learner = createFallbackLearner(task)
  trainWorker(task, fallback.learner, subset)
}

getTrainEvalString = function(encapsulation) {
  assertInt(encapsulation, lower = 1L,  upper = 2L)
  if (encapsulation == 1L) {
    "wrapped.model = trainWorker(task, learner, subset)"
  } else {
     "wrapped.model = callr::r(function(task, learner, subset) {
        library(mlrng)
        mlrng:::trainWorker(task, learner, subset)
        }, list(task = task, learner = learner, subset = subset))"
  }
}
