#' @title Regression Tasks
#' @format \code{\link{R6Class}} object
#'
#' @usage RegrTask$new(...)
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
#' @return [\code{\link{ClassifTask}}].
#' @family Tasks
#' @export
#' @examples
#' task = RegrTask$new(iris, target = "Sepal.Length")
#' task$formula
RegrTask = R6Class("RegrTask",
  inherit = SupervisedTask,
  public = list(
    type = "regr",
    initialize = function(data, target = NA_character_, cols = NULL, id.col = NULL, id = deparse(substitute(data))) {
      super$initialize(data, target = target, cols = cols, id.col = id.col, id = id)
      assertNumeric(self$backend[[target]], finite = TRUE, any.missing = FALSE)
    }
  )
)
