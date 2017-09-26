#' @title WrappedModel
#' @format \code{\link{R6Class}} object
#'
#' @description
#' A \code{\link[R6]{R6Class}} containing data of a \code{\link{train}}.
#'
#' @field task [\code{\link{Task}}]: Task used to fit the model.
#' @field learner [\code{\link{Learner}}]: Learner used to fit the model.
#' @field split [\code{\link{Split}}]: Split into training/test used to fit the model.
#' @field model [any]: Result of the model fit as returned by third party packages.
WrappedModel = R6Class("WrappedModel",
  cloneable = FALSE,
  public = list(
    task = NULL,
    learner = NULL,
    model = NULL,
    split = NULL,
    initialize = function(task, learner, model, split) {
      self$task = assertR6(task, "Task")
      self$learner = assertR6(learner, "Learner")
      self$model = model
      self$split = assertR6(split, "Split")
    }
  )
)
