#' @title Base Class for Tasks
#' @format \code{\link{R6Class}} object
#'
#' @description
#' A \code{\link[R6]{R6Class}} to construct tasks.
#' This is the abstract base class, do not use directly!
#'
#' @template fields-task
#' @field task.type [\code{character(1)}]: Type of task (\dQuote{classif}).
#' @return [\code{\link{Task}}].
#' @family Tasks
Task = R6Class("Task",
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

    truth = function(rows = NULL) {
      self$backend$get(rows, cols = self$target)
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
      if (getOption("mlrng.debug", TRUE))
          cat("\n", format(self), "\n")
  }),

  ### ACTIVE ##################################################################
  active = list(
    data = function(newdata) {
      if (missing(newdata)) {
        return(self$backend$data)
      }
      if (self$backend$writeable) {
        self$backend$data = newdata
      } else {
        self$backend = BackendLocal$new(data = newdata, rowid.col = self$backend$rowid.col)
      }
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

if (FALSE) {
  x = mlr.tasks$get("iris")
  newdata = x$backend$get(include.rowid.col = TRUE)
  x$data = newdata[1:50,]
  x
}
