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
#' @field task.types [\code{character}]: Set of compatible task types.
#' @field fun [\code{function(truth, predicted)}]: function to compute the measure.
#' @return [\code{Measure}].
#' @export
Resampling = R6Class("Resampling",
  public = list(
    id = NA_character_,
    instantiate = NULL,
    iters = NA_integer_,
    checksum = NA_character_,
    pars = list(),
    instance = NULL,

    initialize = function(id, instantiate, iters, pars = list()) {
      self$id = assertString(id)
      if (!is.null(instantiate)) {
        self$instantiate = assertFunction(instantiate, args = "task")
        environment(self$instantiate) = environment(self$initialize)
      }
      self$iters = assertCount(iters)
      self$pars = assertList(pars, names = "unique")
    },

    train.set = function(task, i) {
      if (is.null(self$instance))
        stop("Resampling has not been instantiated yet")
      rows = task$view$active.rows
      bit = self$instance[[i]]$train.set
      assertBit(bit, len = length(bit))
      rows[as.which(bit)]
    },

    test.set = function(task, i) {
      if (is.null(self$instance))
        stop("Resampling has not been instantiated yet")
      rows = task$view$active.rows
      bit = self$instance[[i]]$test.set
      assertBit(bit, len = length(bit))
      rows[as.which(bit)]
    },

    set = function(task, train.sets, test.sets = NULL) {
      if (is.null(test.sets))
        test.sets = lapply(train.sets, function(x) !x)
      self$instance = Map(Split$new, train.set = train.sets, test.set = test.sets)
      self$checksum = digest(list(task$id, task$view$active.rows, self$instance), algo = "murmur32")
      invisible(self)
    },

    reset = function() {
      self$instance = NULL
      self$checksum = NA_character_
      invisible(self)
    },

    print = function(...) {
      if (!self$is.instantiated) cat("(Uninstantiated) ")
      gcat("Resampling: {self$id} with {self$iters} splits.")
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
