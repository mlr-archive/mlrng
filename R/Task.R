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
Task = R6Class("Task",
  public = list(
    ### SLOTS ##################################################################
    id = NULL,
    view = NULL,

    ### METHODS ################################################################
    initialize = function(id, data) {
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

    head = function(n = 6L) {
      setDT(dplyr::collect(head(self$view$tbl, n = n)))[]
    },

    deep_clone = function(name, value) {
      if (name == "view") value$clone(deep = TRUE) else value
    },
    print = function(...) {
      cols = self$col.types
      if(!is.null(self$target))
        cols = cols[names(cols) != self$target]
      tbl = table(cols)
      gcat("Task name: {self$id}
            {self$nrow} rows and {length(cols)} features.
            Features: {stri_peek(names(cols))}")
      cat("Feature types: ")
      gcat("{tbl} {names(tbl)}")
      if (getOption("mlrng.debug", TRUE))
          cat("\n", format(self), "\n")
    }
  ),

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
      res = dplyr::summarize(self$view$tbl, dplyr::funs(sum(is.na(.))))
      unlist(dplyr::collect(res))
    }
  )
)
