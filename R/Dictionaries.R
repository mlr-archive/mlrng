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
mlr.tasks = DictionaryTasks$new()

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
mlr.learners = DictionaryLearners$new()

#' @title Registered Resampling Methods
#' @docType class
#' @format \code{\link{R6Class}} object
#'
#' @description
#' \code{mlr.measures} is a \code{\link{Dictionary}} used to manage performance measures.
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
DictionaryResamplings = R6Class("DictionaryResamplings", inherit = Dictionary,
  public = list(
    initialize = function() {
      super$initialize("Resampling")
    },
    getElementSummary = function(x) {
      data.table(
        id = x$id,
        description = x$description,
        pars = list(x$pars)
      )
    }
  )
)

mlr.resamplings = DictionaryResamplings$new()

