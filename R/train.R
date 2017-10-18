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
  row.ids = translateSubset(task, subset)

  trainWorker(task, learner, row.ids)
}

trainWorker = function(task, learner, row.ids) {
  encapsulation = getOption("mlrng.train.encapsulation", 1L)
  result = NULL

  start.time = proc.time()[3L]
  if (encapsulation == 0L) {
    result = fitModel(task, learner, row.ids)
    raw.log = list()
  } else {
    eval.string = getTrainEvalString(encapsulation)
    raw.log = evaluate::evaluate(eval.string, new_device = FALSE)
  }
  train.time = round(proc.time()[3L] - start.time, 6L)

  train.log = TrainLog$new(raw.log, train.time)
  train.success = !is.null(result) && train.log$n.errors == 0L

  if (train.success) {
    ginfo("Trained {learner$id} on {task$id} with {train.log$n.errors} errors, {train.log$n.warnings} warnings and {train.log$n.messages} messages.")
  } else {
   if (getOption("mlrng.continue.on.learner.error", FALSE)) {
      result = fitModel(task, createFallbackLearner(task), row.ids)
      ginfo("Training {learner$id} on {task$id} failed, fallback to dummy model.")
    } else {
      gstop("Training {learner$id} on {task$id} failed with {train.log$errors[[1]]$message}.")
    }
  }

  TrainResult$new(task, learner, result, row.ids, train.log)
}

fitModel = function(task, learner, row.ids) {
  requireNS(learner$packages)
  pars = c(list(task = task, subset = row.ids), learner$par.vals)
  do.call(learner$train, pars)
}

getTrainEvalString = function(encapsulation) {
  assertInt(encapsulation, lower = 1L,  upper = 2L)
  if (encapsulation == 1L) {
    "result = fitModel(task, learner, row.ids)"
  } else {
     "result = callr::r(function(task, learner, row.ids) {
        library(mlrng)
        mlrng:::fitModel(task, learner, row.ids)
        }, list(task = task, learner = learner, row.ids = row.ids))"
  }
}
