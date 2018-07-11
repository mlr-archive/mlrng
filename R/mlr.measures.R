#' @include Dictionary.R
DictionaryMeasures = R6Class("DictionaryMeasures", inherit = Dictionary,
  public = list(
    initialize = function() {
      super$initialize("Measure")
    },
    summary = function(ids = NULL) {
      extractSummary(self, ids,
        function(obj) list(task.types = list(obj$task.types)))
    }
  )
)

#' @title Registered Resampling Methods
#' @docType class
#' @format [R6Class()] object
#'
#' @description
#' `mlr.measures` is a [Dictionary()] used to manage performance measures.
#'
#' @export
mlr.measures = DictionaryMeasures$new()
