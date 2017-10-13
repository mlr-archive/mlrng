Backend = R6Class("Backend",
  public = list(
    rowid.col = NULL,
    writeable = NULL
  )
)

BackendDBI = R6Class("BackendDBI", inherit = Backend,
  public = list(
    tbl.name = NULL,
    view.rows = NULL,
    view.cols = NULL,
    con.pars = NULL,

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
      } else {
        self$rowid.col = assertString(rowid.col, min.chars = 1L)
        self$con.pars = assertList(data, names = "unique")
      }
      self$writeable = FALSE
    },

    finalizer = function() {
      DBI::dbDisconnect(private$con)
    },

    tbl = function(filter = FALSE, select = FALSE) {
      ok = try(DBI::dbIsValid(private$con), silent = TRUE)
      if (inherits(ok, "try-error") || !isTRUE(ok) || is.null(private$con))
        private$con = do.call(DBI::dbConnect, self$con.pars)
      tbl = dplyr::tbl(self$tbl.name, src = private$con)

      if (filter && !is.null(self$view.rows))
        tbl = dplyr::filter_at(tbl, self$rowid.col, dplyr::all_vars(. %in% self$view.rows[[1L]]))

      if (select && !is.null(self$view.cols))
        tbl = dplyr::select_at(tbl, c(self$rowid.col, self$view.cols))

      return(tbl)
    },

    get = function(rows = NULL, cols = NULL, include.rowid = FALSE) {
      tbl = self$tbl(filter = TRUE, select = TRUE)

      if (!is.null(rows)) {
        tbl = dplyr::filter_at(tbl, self$rowid.col, dplyr::all_vars(. %in% rows))
      }

      if (!is.null(cols)) {
        assertSubset(cols, colnames(tbl))
        tbl = dplyr::select_at(tbl, c(self$rowid.col, cols))
      }

      data = setDT(dplyr::collect(tbl), key = self$rowid.col)
      if (!is.null(rows))
        data = data[list(rows), on = self$rowid.col, nomatch = 0L]
      else if (!is.null(self$view.rows))
        data = data[self$view.rows, on = self$rowid.col, nomatch = 0L]

      if (!is.null(rows) && nrow(data) != length(rows))
        stop("Invalid row ids provided")

      if (!isTRUE(include.rowid))
        data[[self$rowid.col]] = NULL
      return(data)
    },

    subset = function(rows = NULL, cols = NULL) {
      if (!is.null(rows)) {
        assertAtomicVector(rows, any.missing = FALSE)
        self$view.rows = data.table(..id = rows, key = "..id")
        setnames(self$view.rows, "..id", self$rowid.col)
      }

      if (!is.null(cols)) {
        assertSubset(cols, colnames(self$tbl()))
        self$view.cols = setdiff(cols, self$rowid.col)
      }

      invisible(self)
    },

    distinct = function(col) {
      assertChoice(col, self$colnames)
      dplyr::collect(dplyr::distinct(dplyr::select_at(self$tbl(filter = TRUE), col)))[[1L]]
    },

    head = function(n = 6L) {
      setDT(dplyr::collect(head(dplyr::select(self$tbl(filter = TRUE, select = TRUE), -dplyr::one_of(self$rowid.col)), n)))
    }
  ),

  active = list(
    data = function() {
      self$get()
    },

    colnames = function() {
      if (!is.null(self$view.cols))
        return(self$view.cols)
      return(setdiff(colnames(self$tbl()), self$rowid.col))
    },

    rownames = function() {
      if (!is.null(self$view.rows))
        return(self$view.rows[[1L]])
      dplyr::collect(dplyr::select_at(self$tbl(), self$rowid.col))[[1L]]
    },

    nrow = function() {
      if (!is.null(self$view.rows))
        return(nrow(self$view.rows))
      dplyr::collect(dplyr::tally(self$tbl()))[[1L]]
    },

    ncol = function() {
      if (!is.null(self$view.cols))
        return(length(self$view.cols))
      length(colnames(self$tbl())) - 1L
    },

    types = function() {
      vcapply(self$head(1L), class)
    },

    missing.values = function() {
      query = dplyr::summarize_at(self$tbl(filter = TRUE), self$colnames, dplyr::funs(sum(is.na(.))))
      unlist(dplyr::collect(query))
    }
  ),

  private = list(
    con = NULL,
    deep_clone = function(name, value) {
      if (name == "view.rows") {
        if (is.null(value)) NULL else copy(value)
      } else if (name == "con") {
        NULL
      } else {
        value
      }
    }
  )
)

BackendLocal = R6Class("BackendLocal", inherit = Backend,
  public = list(
    internal.data = NULL,
    initialize = function(data, rowid.col = NULL) {
      assertDataFrame(data)
      if (is.null(rowid.col)) {
        data[["..id"]] = seq_len(nrow(data))
        self$rowid.col = "..id"
      } else {
        self$rowid.col = assertChoice(rowid.col, colnames(data))
      }
      self$internal.data = as.data.table(data)
      self$writeable = TRUE
    },

    get = function(rows = NULL, cols = NULL, include.rowid = FALSE) {
      assertSubset(cols, colnames(self$internal.data))
      data = switch(is.null(rows) + 2L * is.null(cols) + 1L,
        self$internal.data[list(rows), c(self$rowid.col, cols), with = FALSE, on = self$rowid.col, nomatch = 0L],
        self$internal.data[, c(self$rowid.col, cols), with = FALSE],
        self$internal.data[list(rows), on = self$rowid.col, nomatch = 0L],
        copy(self$internal.data)
      )
      if (!is.null(rows) && nrow(data) != length(rows))
        stop("Invalid row ids provided")
      if (!include.rowid)
        data[[self$rowid.col]] = NULL
      return(data)
    },

    subset = function(rows = NULL, cols = NULL) {
      self$internal.data = self$get(rows, cols, include.rowid = TRUE)
      invisible(self)
    },

    distinct = function(col) {
      assertChoice(col, self$colnames)
      x = self$internal.data[[col]]
      if (is.factor(x)) levels(x) else unique(x)
    },

    head = function(n = 6L) {
      head(self$internal.data[, !(self$rowid.col), with = FALSE], n)
    }
  ),

  active = list(
    data = function() {
      self$internal.data[, !(self$rowid.col), with = FALSE]
    },

    colnames = function() {
      return(setdiff(colnames(self$internal.data), self$rowid.col))
    },

    rownames = function() {
      return(self$internal.data[[self$rowid.col]])
    },

    nrow = function() {
      return(nrow(self$internal.data))
    },

    ncol = function() {
      return(ncol(self$internal.data) - 1L)
    },

    types = function() {
      vcapply(self$internal.data[, !(self$rowid.col), with = FALSE], class)
    },

    missing.values = function() {
      # FIXME: this copies the data
      return(viapply(self$data, anyMissing))
    }
  )
)
