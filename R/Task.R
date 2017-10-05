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

    head = function(n = 6L) {
      setDT(dplyr::collect(head(self$view$tbl, n = n)))[]
    },

    deep_clone = function(name, value) {
      if (name == "view") value$clone(deep = TRUE) else value
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
