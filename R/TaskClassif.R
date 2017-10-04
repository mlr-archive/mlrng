#' @title Classification Tasks
#' @format \code{\link{R6Class}} object
#'
#' @description
#' A \code{\link[R6]{R6Class}} to construct classification tasks.
#'
#' @template fields-task
#' @template fields-supervisedtask
#' @field type [\code{character(1)}]: Type of task (\dQuote{classif}).
#' @field positive [\code{character(1)}]: Only for binary classification: Level of the positive class (\code{NA} otherwise).
#' @field levels [\code{character()}]: Levels of class labels.
#'
#' @return [\code{\link{TaskClassif}}].
#' @family Tasks
#' @export
#' @examples
#' task = TaskClassif$new("iris", data = iris, target = "Species")
#' task$formula
TaskClassif = R6Class("TaskClassif",
  inherit = TaskSupervised,
  public = list(
    type = "classif",
    positive = NA_character_,
    initialize = function(id = deparse(substitute(data)), data, target, positive) {
      super$initialize(id, data, target)
      # FIXME: we can do this efficiently with dplyr
      target = self$data(cols = self$target)[[1L]]
      qassert(target, c("S", "F"))
    }
  ),

  active = list(
    # FIXME: we can do this efficiently with dplyr
    classes = function() self$view$distinct(self$target),
    nclasses = function() length(self$classes)
  )
)
