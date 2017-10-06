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
    task.type = NULL,
    id = NULL,
    view = NULL,

    ### METHODS ################################################################
    initialize = function(task.type, id, data) {
      self$task.type = assertString(task.type)
      self$id = assertString(id, min.chars = 1L)
      if (is.data.frame(data)) {
        self$view = asView(name = id, data = data)
      } else {
        self$view = assertR6(data, "View")
      }
    },

    data = function(rows = NULL, cols = NULL) {
      setDT(self$view$data(rows, cols))[]
    },

    truth = function(rows = NULL) {
      self$data(rows, cols = self$target)
    },

    head = function(n = 6L) {
      setDT(self$view$head(n))[]
    },

    subset = function(subset) {
      nt = self$clone(deep = TRUE)
      nt$view$active.rows = translateSubset(task, subset)
      return(nt)
    },

    print = function(...) {
      cols = self$col.types
      n.miss = self$na.cols
      n.miss = n.miss[n.miss > 0]
      if(!is.null(self$target))
        cols = cols[names(cols) != self$target]
      tbl = table(cols)
      gcat("Task name: {self$id}
            {self$nrow} rows and {length(cols)} features.
            Features: {stri_peek(names(cols))}
            Feature types: {stri_pasteNames(tbl, names.first = FALSE)}")
      if (length(n.miss > 0))
        gcat("Missings: {stri_pasteNames(n.miss)}")
      if (getOption("mlrng.debug", TRUE))
          cat("\n", format(self), "\n")
  }),

  ### ACTIVE ##################################################################
  active = list(
    nrow = function() {
      self$view$nrow
    },

    ncol = function() {
      self$view$ncol
    },

    col.types = function() {
      self$view$types
    },

    na.cols = function() {
      self$view$na.cols
    }
  )
)
