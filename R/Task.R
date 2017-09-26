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
    hooks = list(),
    initialize = function(id, backend) {
      self$id = assertString(id, min.chars = 1L)
      if (inherits(backend, "DataBackend")) {
        self$backend = backend$clone(deep = TRUE)
      } else {
        assertDataFrame(backend)
        self$backend = DataBackendDataTable$new(backend)
      }
    },

    addHook = function(id, fun, ...) {
      hook = list(list(fun = fun, pars = list(...)))
      self$hooks = c(self$hooks, setNames(hook, id))
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
