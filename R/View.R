View = R6Class("View",
  public = list(
    pars = NULL,
    name = NULL,
    rowid.col = NULL,
    internal.con = NULL,

    initialize = function(pars = list(), name, rowid.col) {
      self$pars = assertList(pars, names = "unique")
      self$name = assertString(name)
      self$rowid.col = assertString(rowid.col)
      private$cache = new.env(parent = emptyenv())
      private$view.cols = setdiff(colnames(self$raw.tbl), rowid.col)
    },

    finalize = function() {
      DBI::dbDisconnect(self$con)
    },

    deep_clone = function(name, value) {
      if (name == "internal.con") NULL else value
    },

    data = function(rows = NULL, cols = NULL) {
      tbl = self$raw.tbl
      tbl = private$filter(tbl, intersect_if_not_null(private$view.rows, rows))
      tbl = private$select(tbl, intersect_if_not_null(private$view.cols, cols))
      dplyr::collect(tbl)
    },

    distinct = function(col) {
      assertChoice(col, self$active.cols)
      private$cached("distinct",
        unlist(dplyr::collect(dplyr::distinct(private$select(private$filter(self$raw.tbl), col))), use.names = FALSE),
        slot = col
      )
    }
  ),

  active = list(
    con = function() {
      ok = try(DBI::dbIsValid(self$internal.con), silent = TRUE)
      if (inherits(ok, "try-error") || !isTRUE(ok) || is.null(self$internal.con))
        self$internal.con = do.call(DBI::dbConnect, self$pars)
      self$internal.con
    },

    tbl = function() {
      private$select(private$filter(self$raw.tbl))
    },

    raw.tbl = function() {
      tbl = dplyr::tbl(self$name, src = self$con)
    },

    active.rows = function(rows) {
      if (missing(rows)) {
        if (!is.null(private$view.rows))
          return(private$view.rows)
        return(private$cached("active.rows",
          dplyr::collect(dplyr::select_at(self$raw.tbl, self$rowid.col))[[1L]]
        ))
      }

      assertAtomicVector(rows, any.missing = FALSE)
      # FIXME: cleanup
      n = dplyr::tally(private$select(private$filter(self$raw.tbl, rows), self$rowid.col))
      if (dplyr::collect(n)[[1L]] != length(rows))
        stop("Invalid row ids provided")

      private$view.rows = rows
      private$cache[["nrow"]] = length(rows)
      private$invalidate(c("active.rows", "distinct"))
    },

    active.cols = function(cols) {
      if (missing(cols)) {
        return(private$view.cols)
      }
      private$view.cols = assertSubset(cols, setdiff(colnames(self$raw.tbl), self$rowid.col))
      private$invalidate("na.cols")
    },

    nrow = function() {
      if (!is.null(private$view.rows))
        return(length(private$view.rows))
      private$cached("nrow",
        dplyr::collect(dplyr::tally(private$filter(self$raw.tbl)))[[1L]]
      )
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
    }
  ),

  private = list(
    view.rows = NULL,
    view.cols = NULL,
    cache = NULL,

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

    filter = function(tbl, rows = private$view.rows) {
      if (!is.null(rows))
        tbl = dplyr::filter_at(tbl, self$rowid.col, dplyr::all_vars(. %in% rows))
      tbl
    },

    select = function(tbl, cols = private$view.cols) {
      if (!is.null(cols))
        tbl = dplyr::select(tbl, dplyr::one_of(cols))
      tbl
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

  view = View$new(
    pars = list(drv = RSQLite::SQLite(), dbname = path, flags = RSQLite::SQLITE_RO),
    name = name,
    rowid.col = rowid.col
  )
}
