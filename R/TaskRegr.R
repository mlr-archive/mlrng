#' @title Regression Tasks
#' @format \code{\link{R6Class}} object
#'
#' @template params-task
#' @template params-supervisedtask
#'
#' @description
#' A \code{\link[R6]{R6Class}} to construct classification tasks.
#'
#' @template fields-task
#' @template fields-supervisedtask
#' @field type [\code{character(1)}]: Type of task (\dQuote{regr}).
#'
#' @return [\code{\link{TaskRegr}}].
#' @family Tasks
#' @export
#' @examples
#' task = TaskRegr$new(iris, target = "Sepal.Length")
#' task$formula
TaskRegr = R6Class("TaskRegr",
  inherit = TaskSupervised,
  public = list(
    type = "regr",
    initialize = function(id, backend, target) {
      super$initialize(id, backend, target)
      assertNumeric(self$targetcol, finite = TRUE, any.missing = FALSE)
    }
  )
)
