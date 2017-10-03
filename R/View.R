intersect_if_not_null = function(x, y) {
  if (!is.null(x)) {
    if (!is.null(y))
      return(intersect(x, y))
    return(x)
  }
  return(y)
}

View = R6Class("View",
  public = list(
    pars = NULL,
    name = NULL,
    rowid.col = NULL,
    internal.con = NULL,

    initialize = function(pars = list(), name, rowid.col = NULL) {
      self$pars = assertList(pars, names = "unique")
      self$name = assertString(name)
      self$rowid.col = assertString(rowid.col)
    },

    deep_clone = function(name, value) {
      if (name == "internal.con") NULL else value
    },

    data = function(rows = NULL, cols = NULL) {
      tbl = self$raw.tbl

      ### select rows first
      rows = intersect_if_not_null(private$rows, rows)
      if (!is.null(rows))
        tbl = dplyr::filter_at(tbl, self$rowid.col, dplyr::all_vars(. %in% rows))

      ### select columns second - we can drop the id col now
      if (is.null(private$view.cols))
        private$view.cols = setdiff(colnames(self$raw.tbl), self$rowid.col)
      cols = intersect_if_not_null(private$view.cols, cols)
      tbl = dplyr::select(tbl, dplyr::one_of(cols))

      dplyr::collect(tbl)
    },

    distinct = function(col) {
      assertChoice(col, self$active.cols)
      dplyr::collect(dplyr::distinct(dplyr::select(self$raw.tbl, col)))[[1L]]
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
      tbl = self$raw.tbl

      if (!is.null(private$view.rows))
        tbl = dplyr::filter_at(tbl, self$rowid.col, dplyr::all_vars(. %in% private$view.rows))

      if (is.null(private$view.cols))
        private$view.cols = setdiff(colnames(self$raw.tbl), self$rowid.col)
      tbl = dplyr::select(tbl, dplyr::one_of(private$view.cols))

      return(tbl)
    },


    raw.tbl = function() {
      tbl = dplyr::tbl(self$name, src = self$con)
    },

    active.rows = function(rows) {
      if (missing(rows)) {
        if (is.null(private$view.rows))
          private$view.rows = dplyr::collect(dplyr::select(self$raw.tbl, dplyr::one_of(self$rowid.col)))[[1L]]
        return(private$view.rows)
      }

      assertAtomicVector(rows, any.missing = FALSE)
      res = dplyr::filter_at(self$raw.tbl, self$rowid.col, dplyr::all_vars(. %in% rows))
      res = dplyr::tally(res)
      res = dplyr::collect(res)
      if (res$n != length(rows))
        stop("Invalid row ids provided")
      private$cache$nrow = length(rows)
      private$view.rows = rows
    },

    active.cols = function(cols) {
      if (missing(cols)) {
        if (is.null(private$view.cols))
          private$view.cols = setdiff(colnames(self$raw.tbl), self$rowid.col)
        return(private$view.cols)
      }
      private$view.cols = assertSubset(cols, setdiff(colnames(self$raw.tbl), self$rowid.col))
    },

    nrow = function() {
      if (!is.null(private$view.rows))
        return(length(private$view.rows))
      if (!is.null(private$cache$nrow))
        return(private$cache$nrow)
      private$cache$nrow = dplyr::collect(dplyr::tally(self$tbl))[[1L]]
    },

    ncol = function() {
      length(private$view.cols)
    },

    types = function() {
      vcapply(dplyr::collect(head(self$tbl, 1L)), class)
    }
  ),

  private = list(
    view.rows = NULL,
    view.cols = NULL,
    cache = list()
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
