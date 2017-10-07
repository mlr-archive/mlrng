#' @title Abstract View on a Data Source
#' @format \code{\link{R6Class}} object
#'
#' @description
#' This class currently sits on top of \pkg{dplyr} and is intended to provide
#' a view on a read-only data source.
#'
#' @field pars [\code{list()}]:\cr
#'   Arguments for \code{\link[DBI]{dbConnect}} to establish a connection and re-connect if
#'   necessary.
#' @field name [\code{character(1)}]:\cr Name of the table in the data base.
#' @field rowid.col [\code{character(1)}]:\cr Name of a unique id column in the table.
#' @field con [\code{connection}]:\cr Connection to the data base.
#' @field raw.tbl [\code{src_sql}]:\cr \pkg{dplyr} source of the data base table.
#' @field tbl [\code{src_sql}]:\cr \pkg{dplyr} source of the data base table, pre-filtered
#'   and pre-selected to the active view.
#' @field data [\code{function(rows = NULL, cols = NULL}]\cr
#'   Function to extract rows and columns from the subset of active rows and active cols.
#' @field active.rows [\code{vector}]\cr
#'   Subset of values in \code{rowid.col}. Can be overwritten to change the view.
#' @field active.cols [\code{vector}]\cr
#'   Subset of column names. Can be overwritten to change the view.
#' @field checksum [\code{character(1)}]\cr
#'   Hash of the active view.
#' @field nrow [\code{integer(1)}]\cr
#'   Number of rows in active view.
#' @field ncol [\code{integer(1)}]\cr
#'   Number of cols in active view.
#' @field head [\code{function(n = 6)}]\cr
#'   Get the first \code{n} rows of the active view.
#' @field types [\code{named character}]\cr
#'   Class information about all columns in the active view.
#' @field na.cols [\code{named integer}]\cr
#'   Number of missing values per column in the active view.
#' @field distinct [\code{function(col)}]\cr
#'   Levels of column \code{col} in the active view.
#'
#' @return [\code{View}].
View = R6Class("View",
  public = list(
    pars = NULL,
    name = NULL,
    rowid.col = NULL,

    initialize = function(pars = list(), name, rowid.col, active.cols = NULL, active.rows = NULL) {
      self$pars = assertList(pars, names = "unique")
      self$name = assertString(name)
      self$rowid.col = assertString(rowid.col)
      private$cache = new.env(hash = FALSE, parent = emptyenv())
      self$active.cols = active.cols
      self$active.rows = active.rows
    },

    finalize = function() {
      DBI::dbDisconnect(self$con)
    },

    data = function(rows = NULL, cols = NULL) {
      tbl = self$raw.tbl

      if (is.null(rows)) {
        tbl = private$filter(tbl, self$active.rows)
        unique.rows = TRUE
      } else {
        select.rows = private$view.rows[list(rows), nomatch = 0L, on = self$rowid.col]
        if (nrow(select.rows) != length(rows))
          stop("Invalid row ids provided")
        select.rows = unique(select.rows, by = self$rowid.col)
        unique.rows = nrow(select.rows) == length(rows)
        tbl = private$filter(tbl, select.rows[[1L]])
      }

      if (is.null(cols)) {
        cols = private$view.cols
      } else {
        assertCharacter(cols, any.missing = FALSE, unique = TRUE)
        cols = intersect(cols, private$view.cols)
      }

      if (unique.rows) {
        data = setDT(dplyr::collect(dplyr::select(tbl, cols)))
      } else {
        data = setDT(dplyr::collect(dplyr::select(tbl, c(self$rowid.col, cols))))
        data = data[rows, !(self$rowid.col), on = self$rowid.col, with = FALSE]
      }

      return(data)
    },

    distinct = function(col) {
      assertChoice(col, private$view.cols)
      private$cached("distinct",
        unlist(dplyr::collect(dplyr::distinct(private$select(private$filter(self$raw.tbl), col))), use.names = FALSE),
        slot = col
      )
    },

    head = function(n = 6L) {
      dplyr::collect(head(private$filter(self$raw.tbl), n = n))
    }
  ),

  active = list(
    con = function() {
      ok = try(DBI::dbIsValid(private$internal.con), silent = TRUE)
      if (inherits(ok, "try-error") || !isTRUE(ok) || is.null(private$internal.con))
        private$internal.con = do.call(DBI::dbConnect, self$pars)
      private$internal.con
    },

    tbl = function() {
      private$select(private$filter(self$raw.tbl))
    },

    raw.tbl = function() {
      tbl = dplyr::tbl(self$name, src = self$con)
    },

    active.rows = function(rows) {
      if (missing(rows))
        return(private$view.rows[[1L]])

      if (is.null(rows)) {
        rows = dplyr::collect(dplyr::select_at(self$raw.tbl, self$rowid.col))[[1L]]
      } else {
        assertAtomicVector(rows, any.missing = FALSE)
        n = dplyr::tally(private$select(private$filter(self$raw.tbl, rows), self$rowid.col))
        if (dplyr::collect(n)[[1L]] != length(rows))
          stop("Invalid row ids provided")
      }

      private$view.rows = data.table(..id = rows, key = "..id")
      setnames(private$view.rows, "..id", self$rowid.col)
      private$invalidate(c("distinct", "na.cols", "checksum"))
    },

    active.cols = function(cols) {
      if (missing(cols))
        return(private$view.cols)

      if (is.null(cols)) {
        private$view.cols = setdiff(colnames(self$raw.tbl), self$rowid.col)
      } else {
        private$view.cols = assertSubset(cols, setdiff(colnames(self$raw.tbl), self$rowid.col))
      }
      private$invalidate(c("types", "na.cols", "checksum"))
    },

    nrow = function() {
      nrow(private$view.rows)
    },

    ncol = function() {
      length(private$view.cols)
    },

    types = function() {
      private$cached("types",
        vcapply(dplyr::collect(head(private$select(self$raw.tbl), 1L)), class)
      )
    },

    na.cols = function() {
      private$cached("na.cols",
        unlist(dplyr::collect(
          dplyr::summarise_at(private$filter(self$raw.tbl), private$view.cols, dplyr::funs(sum(is.na(.))))
        ))
      )
    },

    checksum = function() {
      private$cached("checksum",
        digest(list(self$name, self$active.cols, self$active.rows), algo = "murmur32")
      )
    }
  ),

  private = list(
    view.rows = NULL,
    view.cols = NULL,
    cache = NULL,
    internal.con = NULL,

    cached = function(name, value, slot = NULL) {
      ee = private$cache
      if (is.null(slot)) {
        if (!is.null(ee[[name]]))
          return(ee[[name]])
        ee[[name]] = value
      } else {
        if (is.null(ee[[name]])) {
          ee[[name]] = list()
        } else {
          if (!is.null(ee[[name]][[slot]]))
            return(ee[[name]][[slot]])
        }
        ee[[name]][[slot]] = value
      }
    },

    invalidate = function(name) {
      name = intersect(name, ls(private$cache, all.names = TRUE))
      rm(list = name, envir = private$cache)
    },

    filter = function(tbl, rows = private$view.rows[[1L]]) {
      if (!is.null(rows))
        tbl = dplyr::filter_at(tbl, self$rowid.col, dplyr::all_vars(. %in% rows))
      tbl
    },

    select = function(tbl, cols = private$view.cols) {
      if (!is.null(cols))
        tbl = dplyr::select(tbl, dplyr::one_of(cols))
      tbl
    },

    deep_clone = function(name, value) {
      if (name == "cache") copy_env(value) else value
    }
  )
)

asView = function(name = deparse(substitute(data)), data, rowid.col = NULL, path = NULL, ...) {
  assertString(name, min.chars = 1L)
  assertDataFrame(data)
  if (is.null(rowid.col)) {
    rowid.col = "rowid"
    data[["rowid"]] = seq_len(nrow(data))
  } else {
    assertSubset(rowid.col, names(data))
  }

  if (is.null(path)) {
    path = tempfile(pattern = paste0(name, "_"), fileext = ".sqlite")
  } else {
    assertPathForOutput(path, overwrite = TRUE)
  }
  con = DBI::dbConnect(RSQLite::SQLite(), path)
  dplyr::copy_to(con, data, name = name, temporary = FALSE, overwrite = TRUE, row.names = FALSE, unique_indexes = list(rowid.col))
  DBI::dbDisconnect(con)

  View$new(
    pars = list(drv = RSQLite::SQLite(), dbname = path, flags = RSQLite::SQLITE_RO),
    name = name,
    rowid.col = rowid.col
  )
}
