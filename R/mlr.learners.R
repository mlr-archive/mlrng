#' @include Dictionary.R
DictionaryLearners = R6Class("DictionaryLearners", inherit = Dictionary,
  public = list(
    initialize = function() {
      super$initialize("Learner")
    },

    summary = function(ids = NULL) {
      extractSummary(self, ids,
        function(obj) list(name = obj$name, task.type = obj$task.type, properties = list(obj$properties), packages = list(obj$packages)))
    }
  )
)

#' @title Registered Learners
#' @docType class
#' @format [R6Class()] object
#'
#' @description
#' `mlr.learners` is a [Dictionary()] used to manage learners.
#'
#' @export
#' @examples
#' mlr.learners$ids
#' mlr.learners$contains("classif.dummy")
#' mlr.learners$get("classif.dummy")
mlr.learners = DictionaryLearners$new()
