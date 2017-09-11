#' @title Classification Tasks
#' @format \code{\link{R6Class}} object
#'
#' @usage ClassifTask$new(...)
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
#' @return [\code{\link{ClassifTask}}].
#' @family Tasks
#' @export
#' @examples
#' task = ClassifTask$new(iris, target = "Species")
#' task$formula
ClassifTask = R6Class("ClassifTask",
  inherit = SupervisedTask,
  public = list(
    type = "classif",
    positive = NA_character_,
    initialize = function(data, target, cols = NULL, id.col = NULL, positive = NULL, id = deparse(substitute(data))) {
      super$initialize(data, target = target, cols = cols, id = id, id.col = id.col)
      target = self$backend[[target]]
      assertFactor(target, any.missing = FALSE)
      if (!is.null(positive)) {
        nlevs = nlevels(target)
        if (nlevs > 2L)
          gstop("Cannot set a positive class for multilabel classification with {nlevs} levels")
        self$positive = assertChoice(positive, levels(target))
      }
    }
  ),

  active = list(
    levels = function() levels(self$backend[[self$target]]),
    nlevels = function() nlevels(self$backend[[self$target]])
  )
)
