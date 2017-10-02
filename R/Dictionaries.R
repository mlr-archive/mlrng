#' @include Dictionary.R

#' @title Registered Tasks
#' @docType class
#' @format \code{\link{R6Class}} object
#'
#' @description
#' \code{Tasks} is a \code{\link{Dictionary}} used to manage tasks.
#'
#' @export
DictionaryTasks = R6Class("DictionaryTasks", inherit = Dictionary,
  public = list(
    initialize = function() {
      super$initialize("Measure")
    },
    getElementSummary = function() {
      data.table(
        type = task$type,
        nrow = task$nrow,
        ncol = task$ncol
      )
    }
  )
)
mlr.tasks = DictionaryTasks$new()

#' @title Registered Learners
#' @docType class
#' @format \code{\link{R6Class}} object
#'
#' @description
#' \code{Learners} is a \code{\link{Dictionary}} used to manage learners.
#'
#' @export
#' @examples
#' Learners$ids
#' Learners$contains("classif.dummy")
#' Learners$get("classif.dummy")
DictionaryLearners = R6Class("DictionaryLearners", inherit = Dictionary,
  public = list(
    initialize = function() {
      super$initialize("Learner")
    },
    getElementSummary = function() {
      data.table(
        id = self$id,
        name = self$name,
        type = self$type,
        properties = list(self$properties),
        packages = list(self$packages)
      )
    }
  )
)
mlr.learners = DictionaryLearners$new()

#' @title Registered Resampling Methods
#' @docType class
#' @format \code{\link{R6Class}} object
#'
#' @description
#' \code{Measures} is a \code{\link{Dictionary}} used to manage performance measures.
#'
#' @export
DictionaryMeasures = R6Class("DictionaryMeasures", inherit = Dictionary,
  public = list(
    initialize = function() {
      super$initialize("Measure")
    },
    getElementSummary = function(x) {
      data.table(task.types = list(x$task.types))
    }
  )
)
mlr.measures = DictionaryMeasures$new()


#' @title Registered Resampling Methods
#' @docType class
#' @format \code{\link{R6Class}} object
#'
#' @description
#' \code{Resamplings} is a \code{\link{Dictionary}} used to manage resampling methods.
#'
#' @export
DictionaryResamplings = R6Class("Resamplings", inherit = Dictionary,
  public = list(
    initialize = function() {
      super$initialize("Resampling")
    },
    getElementSummary = function(x) {
      data.table(
        id = r$id,
        description = r$description,
        pars = list(r$pars)
      )
    }
  )
)
mlr.resamplings = DictionaryResamplings$new()

