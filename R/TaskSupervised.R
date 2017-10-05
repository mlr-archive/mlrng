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
    initialize = function(id, data, target) {
      super$initialize(id = id, data = data)
      self$target = assertChoice(target, self$view$active.cols)
    }
  ),

  active = list(
    # [formula]. target ~ x1 + ... + xp
    formula = function() {
      reformulate(self$features, response = self$target)
    },

    # [charvec]. featurenames without targetnames
    features = function() setdiff(self$view$active.cols, self$target)
  )
)

print.TaskSupervised = function(x, debug = getOption("mlrng.debug", FALSE)) {
  gcat("Supervised Task")
  gcat("Target: {x$target}")
  NextMethod()
}
