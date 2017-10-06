#' @include Result.R
#' @title TrainResult
#' @format \code{\link{R6Class}} object
#'
#' @description
#' A \code{\link[R6]{R6Class}} containing data of a \code{\link{train}}.
#'
#' @field task [\code{\link{Task}}]: Task used to fit the model.
#' @field learner [\code{\link{Learner}}]: Learner used to fit the model.
#' @field wrapped.model [any]: Result of the model fit as returned by third party packages.
#' @field train [\code{integer}]: Indices of training data used to fit the model.
#' @field train.log [\code{\link{TrainLog}}]: Logging information from model fit.
#' @field train.success [\code{logical(1)}]: Was the training sucessfull.
#'  Depending on the settings of \code{mlrng.continue.on.train.error} this can still be a valid model, but it uses a dummy fallback learner.
TrainResult = R6Class("TrainResult",
  inherit = Result,
  public = list(
    initialize = function(task, learner, rmodel, train.set, train.log) {
      super$initialize(data.table(
        task = list(assertTask(task)),
        learner = list(assertLearner(learner)),
        rmodel = list(rmodel),
        train.set = list(assertIndexSet(train.set, for.task = task)),
        train.log = list(assertR6(train.log, "TrainLog"))
      ))
    },

    print = function(...) {
      gcat("Training result of {self$learner$id} on {self$task$id}.")
      gcat("Training took {self$train.log$train.time} seconds.")
      if (!self$train.success)
        gcat("Training failed with error {stri_peek(self$train.log$errors[[1]]$message)}.
              Model will output constant predictions from dummy learner.")
      if (getOption("mlrng.debug", TRUE))
        cat("\n", format(self), "\n")
    }
  ),

  active = list(
    task = function() self$data$task[[1L]],
    learner = function() self$data$learner[[1L]],
    rmodel = function() self$data$rmodel[[1L]],
    train.set = function() self$data$train.set[[1L]],
    train.log = function() self$data$train.log[[1L]],
    train.success = function() self$data$train.log[[1L]]$n.errors == 0L
  )
)
