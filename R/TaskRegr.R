#' @title Regression Tasks
#' @format \code{\link{R6Class}} object
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
#' con = ConnectionMem$new("iris", iris)
#' task = TaskRegr$new("iris", con, target = "Sepal.Length")
#' task$formula
TaskRegr = R6Class("TaskRegr",
  inherit = TaskSupervised,
  public = list(
    type = "regr",
    initialize = function(id, connection, target) {
      super$initialize(id, connection, target)
      assertNumeric(self$data(cols = self$target)[[1L]], finite = TRUE, any.missing = FALSE, .var.name = "target column")
    }
  )
)
