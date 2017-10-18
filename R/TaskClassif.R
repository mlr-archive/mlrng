#' @title Classification Tasks
#' @format \code{\link{R6Class}} object
#'
#' @description
#' A \code{\link[R6]{R6Class}} to construct classification tasks.
#'
#' @template fields-task
#' @template fields-supervisedtask
#' @field positive [\code{character(1)}]: Only for binary classification: Level of the positive class (\code{NA} otherwise).
#' @field classes [\code{character()}]: Levels of class labels.
#' @field nclasses [\code{integer(1)}]: Number of levels of class labels.
#'
#' @return [\code{\link{TaskClassif}}].
#' @include TaskSupervised.R
#' @export
#' @family Tasks
#' @examples
#' task = TaskClassif$new("iris", data = iris, target = "Species")
#' task$formula
TaskClassif = R6Class("TaskClassif",
  inherit = TaskSupervised,
  public = list(
    task.type = "classif",
    positive = NA_character_,
    initialize = function(id = deparse(substitute(data)), data, target, positive) {
      super$initialize(id = id, data = data, target = target)
      qassert(self$head(1L)[[self$target]], c("S", "F"))
    },
    print = function(...) {
      cat("Classification ")
      super$print()
    }
  ),

  active = list(
    classes = function() self$backend$distinct(self$target),
    nclasses = function() length(self$classes)
  )
)
