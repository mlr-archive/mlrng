#' @title Base Class for Resampling Measures
#' @format [R6Class()] object
#'
#' @description
#' A [R6::R6Class()] to construct resampling measures.
#' This is the abstract base class, do not use directly!
#'
#' Predefined resampling measures are stored in [mlr.resamplings()].
#'
#' @field id [`character(1)`]: Identifier of the measure.
#' @field task.types [`character`]: Set of compatible task types.
#' @field fun [`function(truth, predicted)`]: function to compute the measure.
#' @return [`Measure`].
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

    train.set = function(i) {
      if (is.null(self$instance))
        stop("Resampling has not been instantiated yet")
      assertInt(i, lower = 1L, upper = self$iters)
      self$instance$train[[i]]
    },

    test.set = function(i) {
      if (is.null(self$instance))
        stop("Resampling has not been instantiated yet")
      assertInt(i, lower = 1L, upper = self$iters)
      self$instance$test[[i]]
    },

    set = function(task, train.sets, test.sets = NULL) {
      assertList(train.sets, len = self$iters)
      qassertr(train.sets, sprintf("X[1, %i]", task$nrow))
      if (is.null(test.sets)) {
        test.sets = lapply(train.sets, setdiff, x = seq_len(task$nrow))
      } else {
        assertList(test.sets, len = self$iters)
        qassertr(test.sets, sprintf("X[1, %i]", task$nrow))
      }

      rows = task$backend$rownames
      self$instance = list(
        train = lapply(train.sets, function(i) rows[i]),
        test  = lapply(test.sets, function(i) rows[i])
      )
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
      gcat("Resampling: {self$id} with {self$iters} splits.")
    }
  ),
  active = list(
    is.instantiated = function() {
      !is.null(self$instance)
    })
)
