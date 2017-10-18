#' @title Regression Tasks
#' @format \code{\link{R6Class}} object
#'
#' @description
#' A \code{\link[R6]{R6Class}} to construct classification tasks.
#'
#' @template fields-task
#' @template fields-supervisedtask
#'
#' @return [\code{\link{TaskRegr}}].
#' @include TaskSupervised.R
#' @family Tasks
#' @export
#' @examples
#' task = TaskRegr$new(data = iris, target = "Sepal.Length")
#' task$formula
TaskRegr = R6Class("TaskRegr",
  inherit = TaskSupervised,
  public = list(
    task.type = "regr",
    initialize = function(id = deparse(substitute(data)), data, target) {
      assertString(target)
      super$initialize(id = id, data = data, target = target)
      assertNumeric(self$truth()[[1L]], finite = TRUE, any.missing = FALSE, .var.name = "target column")
    },
    print = function(...) {
      cat("Regression ")
      super$print()
    }
  )
)
