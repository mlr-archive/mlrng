BackendDplyr = R6Class("Backend",
  public = list(
    primary.key = NULL,

    initialize = function(data, primary.key) {
      requireNamespaces(c("dplyr", "DBI", "dbplyr"),
        "The following packages are unavailable for this backend: %s")
      if (!dplyr::is.tbl(data))
        stop("Argument data must be of class 'tbl'")

      assertString(primary.key)
      assertNames(colnames(data), must.include = primary.key)
      assertAtomicVector(dplyr::collect(dplyr::select_at(data, primary.key))[[1L]], any.missing = FALSE, unique = TRUE)
      self$primary.key = primary.key

      private$tbl = data
    },

    data = function(rows, cols) {
      assertAtomicVector(rows)
      assertNames(cols, type = "unique", subset.of = names(private$dt))

      duplicated.rows = anyDuplicated(rows) > 0L
      extra.cols = if (duplicated.rows) setdiff(self$primary.key, cols) else character(0L)

      res = setDT(dplyr::collect(dplyr::select_at(
          dplyr::filter_at(private$tbl, self$primary.key, dplyr::all_vars(. %in% rows)),
          c(extra.cols, cols))))

      if (duplicated.rows) res[list(rows), cols, on = self$primary.key, with = FALSE] else res
    },

    head = function(n = 6L) {
      setDT(dplyr::collect(head(private$tbl, n)))[]
    }
  ),

  active = list(
    colnames = function() {
      colnames(private$tbl)
    },

    rownames = function() {
      dplyr::collect(dplyr::select_at(private$tbl, self$primary.key))[[1L]]
    },

    nrow = function() {
      dplyr::collect(dplyr::tally(private$tbl))[[1L]]
    },

    ncol = function() {
      ncol(private$tbl)
    }
  ),

  private = list(
    tbl = NULL,
    reconnect = function() {
      if (!DBI::dbIsValid(private$tbl$src$con)) {
        # FIXME:
      }
    }
  )
)

BackendDataTable = R6Class("Backend",
  public = list(
    primary.key = NULL,

    initialize = function(data, primary.key = NULL) {
      assertDataFrame(data, min.rows = 1L, min.cols = 1L)

      if (is.null(primary.key)) {
        rn = attr(data, "row.names")
        data = as.data.table(data)
        if (is.character(rn))
          rn = make.unique(rn)
        data[["..row.id"]] = rn
        self$primary.key = "..row.id"
      } else {
        assertString(primary.key)
        assertNames(colnames(data), must.include = primary.key)
        assertAtomicVector(data[[primary.key]], any.missing = FALSE, unique = TRUE)
        self$primary.key = primary.key
        data = as.data.table(data)
      }
      private$dt = setkeyv(data, private$primary.key)
    },

    data = function(rows, cols) {
      assertAtomicVector(rows)
      assertNames(cols, type = "unique", subset.of = names(private$dt))

      data = private$dt[list(rows), cols, with = FALSE, nomatch = 0L, on = self$primary.key]
      return(data)
    },

    head = function(n = 6L) {
      head(private$dt, n)
    }
  ),

  active = list(
    colnames = function() {
      colnames(private$dt)
    },

    rownames = function() {
      private$dt[[self$primary.key]]
    },

    nrow = function() {
      nrow(private$dt)
    },

    ncol = function() {
      ncol(private$dt)
    }
  ),

  private = list(
    dt = NULL,

    deep_clone = function(name, value) {
      if (name == "dt") copy(name) else value
    }
  )
)

#' @export
as.data.table.Backend = function(x) {
  x$data(x$rownames, x$colnames)
}
