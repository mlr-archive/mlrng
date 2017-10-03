#' @title MlrModel
#' @format \code{\link{R6Class}} object
#'
#' @description
#' A \code{\link[R6]{R6Class}} containing data of a \code{\link{train}}.
#'
#' @field task [\code{\link{Task}}]: Task used to fit the model.
#' @field learner [\code{\link{Learner}}]: Learner used to fit the model.
#' @field wrapped.model [any]: Result of the model fit as returned by third party packages.
#' @field train [\code{integer}]: Indices of training data used to fit the model.
#' @field log [\code{\link{TrainLog}}]: Logging information from model fit.
#' @field train.success [\code{logical(1)}]: Was the training sucessfull.
#'  Depending on the settings of \code{mlrng.continue.on.train.error} this can still be a valid model, but it uses a dummy fallback learner.
MlrModel = R6Class("MlrModel",
  cloneable = FALSE,
  public = list(
    task = NULL,
    learner = NULL,
    wrapped.model = NULL,
    train = NULL,
    log = NULL,
    train.success = NULL,
    initialize = function(task, learner, wrapped.model, train, log, train.success) {
      self$task = assertR6(task, "Task")
      self$learner = assertR6(learner, "Learner")
      self$wrapped.model = wrapped.model
      self$train = assertInteger(train)
      self$log = assertR6(log, "TrainLog")
      self$train.success = assertFlag(train.success)
    }
  )
)
