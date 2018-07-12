#' @title Base Class for Measures
#' @format [R6Class()] object
#'
#' @description
#' A [R6::R6Class()] to construct performance measures.
#' This is the abstract base class, do not use directly!
#'
#' Predefined measures are stored in [mlr.measures()].
#'
#' @field id (`character(1)`): Identifier of the measure.
#' @field description (`character(1)`): Description of the measure.
#' @field task.types (`character(1)`): Set of compatible task types.
#' @field fun [`function(truth, predicted)`]: function to compute the measure.
#' @return [`Measure`].
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
    },
    print = function(...) {
      gcat("Measure {self$id}
            Compatible task types: {stri_paste(self$task.types, collapse = ', ')}
            Description: {stri_peek(self$description)}")
      if (getOption("mlrng.debug", FALSE))
        cat("\n", format(self), "\n")
    }
  )
)
