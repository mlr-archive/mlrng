#' @title Base Class for Resampling Measures
#' @format \code{\link{R6Class}} object
#'
#' @description
#' A \code{\link[R6]{R6Class}} to construct resampling measures.
#' This is the abstract base class, do not use directly!
#'
#' Predefined resampling measures are stored in \code{\link{mlr.resamplings}}.
#'
#' @field id [\code{character(1)}]: Identifier of the measure.
#' @field description [\code{character(1)}]: Name of the measure.
#' @field task.types [\code{character}]: Set of compatible task types.
#' @field fun [\code{function(truth, predicted)}]: function to compute the measure.
#' @return [\code{Measure}].
#' @export
Resampling = R6Class("Resampling",
  public = list(
    id = NA_character_,
    description = NA_character_,
    instantiate = NULL,
    iters = NA_integer_,
    checksum = NA_character_,
    pars = list(),
    instance = NULL,

    initialize = function(id, description, instantiate, iters, pars = list()) {
      self$id = assertString(id)
      self$description = assertString(description)
      if (!is.null(instantiate)) {
        self$instantiate = assertFunction(instantiate, args = "task")
        environment(self$instantiate) = environment(self$initialize)
      }
      self$iters = assertCount(iters)
      self$pars = assertList(pars, names = "unique")
    },

    split = function(i) {
      assertInt(i, lower = 1L, upper = self$iters)
      self$instance[[i]]
    },

    train = function(i) {
      if (is.null(self$instance))
        stop("Resampling has not been instantiated yet")
      self$instance[[i]][["train"]]
    },

    test = function(i) {
      if (is.null(self$instance))
        stop("Resampling has not been instantiated yet")
      self$instance[[i]][["test"]]
    },

    set = function(task, train, test = NULL) {
      if (is.null(test))
        test = lapply(train, function(x) !x)
      ids = task$view$active.rows
      train = lapply(train, function(x) ids[x])
      test = lapply(test, function(x) ids[x])

      self$instance = Map(Split$new, train = train, test = test)
      self$checksum = digest(self$instance, algo = "murmur32")
      invisible(self)
    },

    reset = function() {
      self$instance = NULL
      self$checksum = NA_character_
      invisible(self)
    },
    print = function(...) {
      if (!self$is.instantiated) cat("(Uninstantiated) ")
      gcat("Resampling: {self$description} [{self$id}] with {self$iters} splits.")
    }
  ),
  active = list(
    is.instantiated = function() {
      !is.null(self$instance)
    })
)

#' @export
as.data.table.Resampling = function(x, keep.rownames = FALSE, ...) {
  data.table(
    iter = seq_along(x),
    split = x$instance
  )
}
