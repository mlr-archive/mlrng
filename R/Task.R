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
    con = NULL,
    desc = list(),

    ### METHODS ################################################################
    initialize = function(id, connection = NULL) {
      self$id = assertString(id, min.chars = 1L)
      self$con = assertR6(connection, "Connection")
      private$view.cols = setdiff(colnames(self$con$tbl), self$con$rowid.col)
      private$cache = new.env(parent = emptyenv())
    },


    data = function(rows = NULL, cols = NULL) {
      res = self$con$tbl
      n = is.null(rows) + is.null(private$view.rows)
      if (n < 2L) {
        ids = if (n == 0L) intersect(rows, private$view.rows) else rows %??% private$view.rows
        if (length(ids)) {
          res = dplyr::filter_at(res, self$con$rowid.col, dplyr::all_vars(. %in% ids))
        } else {
          # filter with %in% on empty vector seems to be buggy?
          res = dplyr::filter(res, FALSE)
        }
      }

      cols = if (is.null(cols)) private$view.cols else intersect(cols, private$view.cols)
      cols = setdiff(cols, self$con$rowid.col)
      res = dplyr::select(res, dplyr::one_of(cols))
      setDT(dplyr::collect(res))[]
    },

    head = function(n) {
      res = dplyr::select(self$con$tbl, setdiff(private$view.cols, self$con$rowid.col))
      res = head(res, assertCount(n))
      setDT(dplyr::collect(res))[]
    },

    all.rows = function() {
      res = dplyr::select(self$con$tbl, dplyr::one_of(self$con$rowid.col))
      dplyr::collect(res)[[1L]]
    },

    all.cols = function() {
      colnames(self$con$tbl)
    },

    levels = function(col) {
      assertString(col)
      cached = private$cache$levels[[col]]
      if (!is.null(cached))
        return(cached)
      if (is.null(private$cache$levels))
        private$cache$levels = list()
      res = dplyr::collect(dplyr::distinct(dplyr::select(self$con$tbl, col)))[[1L]]
      private$cache$levels[[col]] = res
      return(res)
    }
  ),

  ### ACTIVE ##################################################################
  active = list(
    active.rows = function(rows) {
      if (missing(rows))
        return(private$view.rows %??% self$all.rows())
      private$view.rows = assertSubset(rows, self$all.rows())
    },

    active.cols = function(cols) {
      if (missing(cols))
        return(private$view.cols)
      private$view.cols = assertSubset(cols, self$all.cols())
    },

    nrow = function() {
      cached = private$cache$nrow
      if (!is.null(cached))
        return(cached)
      private$cache$nrow = if (!is.null(private$view.rows))
         length(private$view.rows)
      else
        dplyr::collect(dplyr::tally(self$con$tbl))[[1L]]
    },

    ncol = function() {
      length(private$view.cols)
    },

    col.types = function() {
      cached = private$cache$types
      if (!is.null(cached))
        return(cached)
      private$cache$types = vcapply(self$head(1L), class)
    },

    na.cols = function() {
      res = dplyr::summarize_at(self$con$tbl, self$active.cols, dplyr::funs(sum(is.na(.))))
      unlist(dplyr::collect(res))
    }
  ),

  ### PRIVATE #################################################################
  private = list(
    view.rows = NULL,
    view.cols = NULL,
    cache = NULL
  )
)
