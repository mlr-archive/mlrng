#' @title Regression Tasks
#' @format [R6Class()] object
#'
#' @description
#' A [R6::R6Class()] to construct regression tasks.
#'
#' @template fields-task
#' @template fields-supervisedtask
#'
#' @return [TaskRegr()].
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
