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
  inherit = R6DT,
  public = list(
    #FIXME: train.success seems clumsy name
    initialize = function(task, learner, rmodel, train.set, train.log, train.success) {
      super$initialize(
        task = assertTask(task),
        learner = assertLearner(learner),
        rmodel = rmodel,
        train.set = assertIndexSet(train.set, for.task = task),
        train.log = assertR6(train.log, "TrainLog"),
        train.success = assertFlag(train.success)
      )
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
    task = function() self$dt$task[[1L]],
    learner = function() self$dt$learner[[1L]],
    rmodel = function() self$dt$rmodel[[1L]],
    train.set = function() self$dt$train.set[[1L]],
    train.log = function() self$dt$train.log[[1L]],
    train.success = function() self$dt$train.success[[1L]]
  )
)
