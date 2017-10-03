#' @title Base Class for Measures
#' @format \code{\link{R6Class}} object
#'
#' @description
#' A \code{\link[R6]{R6Class}} to construct performance measures.
#' This is the abstract base class, do not use directly!
#'
#' Predefined measures are stored in \code{\link{mlr.measures}}.
#'
#' @field id [\code{character(1)}]: Identifier of the measure.
#' @field description [\code{character(1)}]: Description of the measure.
#' @field task.types [\code{character}]: Set of compatible task types.
#' @field fun [\code{function(truth, predicted)}]: function to compute the measure.
#' @return [\code{Measure}].
#' @export
Measure = R6Class("Measure",
  public = list(
    id = NULL,
    description = NULL,
    task.types = character(0L),
    fun = NULL,
    initialize = function(id, description, task.types, fun) {
      self$id = assertString(id, min.chars = 1L)
      self$description = assertString(description, min.chars = 1L)
      self$task.types = assertCharacter(task.types, min.len = 1L, any.missing = FALSE)
      self$fun = assertFunction(fun)
      environment(self$fun) = environment(self$initialize)
    }
  )
)
