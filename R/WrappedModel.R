#' @title WrappedModel
#' @format \code{\link{R6Class}} object
#'
#' @description
#' A \code{\link[R6]{R6Class}} containing data of a \code{\link{train}}.
#'
#' @field task [\code{\link{Task}}]: Task used to fit the model.
#' @field learner [\code{\link{Learner}}]: Learner used to fit the model.
#' @field resampling [\code{\link{Resampling}}]: Resampling used to fit the model.
#' @field model [any]: Result of the model fit as returned by third party packages.
WrappedModel = R6Class("WrappedModel",
  cloneable = FALSE,
  public = list(
    task = NULL,
    model = NULL,
    learner = NULL,
    resampling = NULL,
    initialize = function(task, learner, model, resampling) {
      self$task = assertR6(task, "Task")
      self$learner = assertR6(learner, "Learner")
      self$model = model
      self$resampling = assertR6(resampling, "Resampling")
    }
  )
)
