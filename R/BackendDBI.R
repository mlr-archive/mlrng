#' @include Backend.R
BackendDBI = R6Class("BackendDBI", inherit = Backend,
  public = list(
    tbl.name = NULL,
    con.pars = NULL,
    transformators = list(),

    initialize = function(data, rowid.col = NULL, tbl.name) {
      self$tbl.name = assertString(tbl.name, min.chars = 1L)

      if (is.data.frame(data)) {
        if (is.null(rowid.col)) {
          self$rowid.col = "..id"
          data[["..id"]] = seq_len(nrow(data))
        } else {
          self$rowid.col = assertChoice(rowid.col, colnames(data))
        }

        path = tempfile(pattern = paste0(tbl.name, "_"), fileext = ".sqlite")
        assertPathForOutput(path, overwrite = TRUE)
        con = DBI::dbConnect(RSQLite::SQLite(), path, flags = RSQLite::SQLITE_RWC)
        dplyr::copy_to(con, data, name = tbl.name, temporary = FALSE, overwrite = TRUE, row.names = FALSE, unique_indexes = list(self$rowid.col))
        DBI::dbDisconnect(con)
        self$con.pars = list(drv = RSQLite::SQLite(), dbname = path, flags = RSQLite::SQLITE_RO)
        self$transformators = getDefaultTransformators(data)
      } else {
        self$rowid.col = assertString(rowid.col, min.chars = 1L)
        self$con.pars = assertList(data, names = "unique")
      }
    },

    finalizer = function() {
      DBI::dbDisconnect(private$con)
    },


    get = function(rows, cols) {
      tbl = self$tbl
      assertAtomicVector(rows)
      assertSubset(cols, colnames(tbl))

      tbl = dplyr::filter_at(tbl, self$rowid.col, dplyr::all_vars(. %in% rows))
      tbl = dplyr::select_at(tbl, union(self$rowid.col, cols))
      data = setDT(dplyr::collect(tbl), key = self$rowid.col)

      if (anyDuplicated(rows))
        data = data[.(rows), on = self$rowid.col, nomatch = 0L]
      if (self$rowid.col %nin% cols)
        data[[self$rowid.col]] = NULL
      if (nrow(data) != length(rows))
        stop("Invalid row ids provided")
      if (ncol(data) != length(cols))
        stop("Invalid col ids provided")

      return(private$transform(data))
    },

    distinct = function(col) {
      tbl = self$tbl
      assertChoice(col, colnames(tbl))
      x = private$transform(setDT(dplyr::collect(dplyr::distinct(dplyr::select_at(tbl, col)))))[[1L]]
      if (is.factor(x))
        return(as.character(unique(x)))
      return(unique(x))
    },

    missing.values = function(col) {
      tbl = self$tbl
      assertChoice(col, colnames(tbl))
      dplyr::collect(dplyr::summarize_at(tbl, col, dplyr::funs(sum(is.na(.)))))[[1L]]
    },

    head = function(n = 6L) {
      tab = dplyr::collect(head(self$tbl, n))
      private$transform(setDT(tab)[])
    }
  ),

  active = list(
    tbl = function() {
      if (is.null(private$con)) {
        private$con = do.call(DBI::dbConnect, self$con.pars)
      } else {
        ok = try(DBI::dbIsValid(private$con), silent = TRUE)
        if (inherits(ok, "try-error") || !isTRUE(ok)) {
          suppressWarnings(DBI::dbDisconnect(private$con))
          private$con = do.call(DBI::dbConnect, self$con.pars)
        }
      }

      dplyr::tbl(self$tbl.name, src = private$con)
    },

    data = function(newdata) {
        return(setDT(dplyr::collect(self$tbl), key = self$rowid.col))
      stop("Cannot write to DBI backend")
    },

    colnames = function() {
      colnames(self$tbl)
    },

    rownames = function() {
      dplyr::collect(dplyr::select_at(self$tbl, self$rowid.col))[[1L]]
    },

    nrow = function() {
      dplyr::collect(dplyr::tally(self$tbl))[[1L]]
    },

    ncol = function() {
      ncol(self$tbl)
    }
  ),

  private = list(
    con = NULL,

    deep_clone = function(name, value) {
      if (name == "con") NULL else  value
    },

    transform = function(data) {
      nms = intersect(names(self$transformators), names(data))
      for (n in nms)
        set(data, j = n, value = self$transformators[[n]](data[[n]]))
      data
    }
  )
)

getDefaultTransformators = function(data) {
  getTrafo = function(x) {
    switch(class(x),
      character = function(x) as.character(x),
      factor = function(x) factor(x),
      integer = function(x) as.integer(x),
      logical = function(x) as.logical(x),
      NULL
    )
  }
  filterNull(lapply(data, getTrafo))
}
