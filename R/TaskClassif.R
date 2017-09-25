#' @title Classification Tasks
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
#' @field type [\code{character(1)}]: Type of task (\dQuote{classif}).
#' @field positive [\code{character(1)}]: Only for binary classification: Level of the positive class (\code{NA} otherwise).
#' @field levels [\code{character()}]: Levels of class labels.
#'
#' @return [\code{\link{TaskClassif}}].
#' @family Tasks
#' @export
#' @examples
#' task = TaskClassif$new(iris, target = "Species")
#' task$formula
TaskClassif = R6Class("TaskClassif",
  inherit = TaskSupervised,
  public = list(
    type = "classif",
    positive = NA_character_,
    initialize = function(id, backend, target, positive = NULL) {
      super$initialize(id, backend, target)
      target = self$backend$get(cols = self$target)[[1L]]
      assertFactor(target, any.missing = FALSE)
      if (!is.null(positive)) {
        nlevs = nlevels(target)
        if (nlevs > 2L)
          gstop("Cannot set a positive class for multiclass classification with {nlevs} levels")
        self$positive = assertChoice(positive, levels(target))
      }
    }
  ),

  active = list(
    classes = function() levels(self$backend$get(cols = self$target)[[1L]]),
    nclasses = function() length(self$classes)
  )
)
