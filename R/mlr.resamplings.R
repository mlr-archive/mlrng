#' @include Dictionary.R
DictionaryResamplings = R6Class("DictionaryResamplings", inherit = Dictionary,
  public = list(
    initialize = function() {
      super$initialize("Resampling")
    },
    summary = function(ids = NULL) {
      extractSummary(self, ids,
        function(obj) list(pars = list(obj$pars)))
    }
  )
)

#' @title Registered Resampling Methods
#' @docType class
#' @format \code{\link{R6Class}} object
#'
#' @description
#' \code{Resamplings} is a \code{\link{Dictionary}} used to manage resampling methods.
#'
#' @export
mlr.resamplings = DictionaryResamplings$new()
