#' @title Basic Tasks
#' @format [R6Class()] object
#'
#' @description
#' This is the abstract base class for task objects.
#' Use [TaskClassif()] or [TaskRegr()] to construct tasks instead of this class.
#'
#' @template fields-task
#' @template fields-supervisedtask
#'
#' @return [[Task()]].
#' @export
#' @family Tasks
#' @examples
#' task = Task$new("iris", data = iris)
#' task$formula
Task = R6Class("Task",
  # Base Class for Tasks
  public = list(
    ### SLOTS ##################################################################
    task.type = NA_character_,
    id = NULL,
    backend = NULL,

    ### METHODS ################################################################
    initialize = function(id, data) {
      self$id = assertString(id, min.chars = 1L)
      if (is.data.frame(data)) {
        self$backend = BackendDBI$new(data = data, tbl.name = id)
      } else {
        self$backend = assertR6(data, "Backend")
      }
    },

    get = function(rows = NULL, cols = NULL) {
      self$backend$get(rows = rows, cols = cols)
    },

    head = function(n = 6L) {
      assertCount(n)
      self$backend$head(n)
    },

    subset = function(rows = NULL, cols = NULL) {
      self$backend$subset(rows, cols)
      invisible(self)
    },

    print = function(...) {
      cols = self$col.types
      if(hasName(self, "target"))
        cols = cols[names(cols) != self$target]
      tbl = table(cols)
      gcat("Task name: {self$id}
            {self$nrow} rows and {length(cols)} features.
            Features: {stri_peek(names(cols))}
            Feature types: {stri_pasteNames(tbl, names.first = FALSE)}
            Missings: {any(self$backend$missing.values) > 0L}")
      if (getOption("mlrng.debug", FALSE))
          cat("\n", format(self), "\n")
  }),

  ### ACTIVE ##################################################################
  active = list(
    data = function(newdata) {
      if (missing(newdata)) {
        return(self$backend$data)
      }
      if (inherits(self$backend, "BackendLocal")) {
        self$backend$data = newdata
      } else {
        if (getOption("mlrng.debug"))
          gmessage("Creating an in-memory copy of task '{self$id}'")
        self$backend = BackendLocal$new(data = newdata, rowid.col = self$backend$rowid.col)
      }
    },

    # [charvec]. feature names without target names
    features = function() {
      self$backend$colnames
    },

    formula = function() {
      reformulate(self$features)
    },

    nrow = function() {
      self$backend$nrow
    },

    ncol = function() {
      self$backend$ncol
    },

    col.types = function() {
      self$backend$types
    },

    missing.values = function() {
      self$backend$missing.values
    }
  )
)
