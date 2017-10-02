#' @title Base Class for Supervised Tasks
#'
#' @description
#' A \code{\link[R6]{R6Class}} to construct supervised tasks.
#' This is the abstract base class, do not use directly!
#'
#' @template fields-task
#' @template fields-supervisedtask
#' @return [\code{\link{TaskSupervised}}].
#' @family Tasks
#' @include Task.R
#' @export
TaskSupervised = R6Class("TaskSupervised",
  inherit = Task,
  public = list(
    target = NA_character_,
    initialize = function(id, connection, target) {
      super$initialize(id = id, connection = connection)
      self$target = assertChoice(target, self$active.cols)
    }
  ),

  active = list(
    # [formula]. target ~ x1 + ... + xp
    formula = function() {
      reformulate(setdiff(self$active.cols, self$target), response = self$target)
    },

    # [charvec]. featurenames without targetnames
    features = function() setdiff(self$active.cols, self$target)
  )
)
