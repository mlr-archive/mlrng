#' @include Dictionary.R

DictionaryTasks = R6Class("DictionaryTasks", inherit = Dictionary,
  public = list(
    initialize = function() {
      super$initialize("Task")
    },
    getElementSummary = function(x) {
      data.table(
        task.type = x$task.type,
        nrow = x$nrow,
        ncol = x$ncol
        )
    }
  )
)

#' @title Registered Tasks
#' @docType class
#' @format \code{\link{R6Class}} object
#'
#' @description
#' \code{Tasks} is a \code{\link{Dictionary}} used to manage tasks.
#'
#' @export
mlr.tasks = DictionaryTasks$new()

DictionaryLearners = R6Class("DictionaryLearners", inherit = Dictionary,
  public = list(
    initialize = function() {
      super$initialize("Learner")
    },
    getElementSummary = function(x) {
      data.table(
        name = x$name,
        task.type = x$task.type,
        properties = list(x$properties),
        packages = list(x$packages)
      )
    }
  )
)

#' @title Registered Learners
#' @docType class
#' @format \code{\link{R6Class}} object
#'
#' @description
#' \code{mlr.learners} is a \code{\link{Dictionary}} used to manage learners.
#'
#' @export
#' @examples
#' mlr.learners$ids
#' mlr.learners$contains("classif.dummy")
#' mlr.learners$get("classif.dummy")
mlr.learners = DictionaryLearners$new()


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

#' @title Registered Resampling Methods
#' @docType class
#' @format \code{\link{R6Class}} object
#'
#' @description
#' \code{mlr.measures} is a \code{\link{Dictionary}} used to manage performance measures.
#'
#' @export
mlr.measures = DictionaryMeasures$new()

DictionaryResamplings = R6Class("DictionaryResamplings", inherit = Dictionary,
  public = list(
    initialize = function() {
      super$initialize("Resampling")
    },
    getElementSummary = function(x) {
      data.table(
        id = x$id,
        pars = list(x$pars)
      )
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
