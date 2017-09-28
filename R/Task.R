#' @title Base Class for Tasks
#' @format \code{\link{R6Class}} object
#'
#' @description
#' A \code{\link[R6]{R6Class}} to construct tasks.
#' This is the abstract base class, do not use directly!
#'
#' @template fields-task
#' @return [\code{\link{Task}}].
#' @family Tasks
#' @export
Task = R6Class("Task",
  public = list(
    backend = NULL,
    id = NULL,
    initialize = function(id, data) {
      self$id = assertString(id, min.chars = 1L)
      if (inherits(data, "DataBackend")) {
        self$backend = data$clone(deep = TRUE)
      } else {
        assertDataFrame(data)
        self$backend = DataBackendDataTable$new(data)
      }
    }
  ),

  active = list(
    features = function() self$backend$cols,
    formula = function() reformulate(self$backend$cols)
  ),

  private = list(
    deep_clone = function(name, value) {
      if (name == "backend") value$clone(deep = TRUE) else value
    }
  )
)
