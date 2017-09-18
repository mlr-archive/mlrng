#' @title Base Class for Supervised Tasks
#'
#' @template params-task
#' @template params-supervisedtask
#'
#' @description
#' A \code{\link[R6]{R6Class}} to construct supervised tasks.
#' This is the abstract base class, do not use directly!
#'
#' @template fields-task
#' @template fields-supervisedtask
#' @return [\code{\link{SupervisedTask}}].
#' @family Tasks
#' @include Task.R
#' @export
SupervisedTask = R6Class("SupervisedTask",
  inherit = Task,
  public = list(
    target = NA_character_,
    initialize = function(data, target, cols = NULL, id.col = NULL, id = deparse(substitute(data))) {
      super$initialize(data, cols = cols, id.col = id.col, id = id)
      self$target = assertChoice(target, self$backend$cols)
    }
  ),
  active = list(
    # [formula]. target ~ x1 + ... + xp
    formula = function() {
      reformulate(setdiff(self$backend$cols, self$target), response = self$target)
    },
    # [charvec]. featurenames without targetnames
    features = function() setdiff(self$backend$cols, self$target),
    # [vec]. targetvalues
    targets = function() self[[self$target]]
  )
)
