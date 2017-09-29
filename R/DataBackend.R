DataBackend = R6Class("DataBackend",
  public = list(
    rowid.col = NULL # [string], col in data that stores row ids
  ),

  active = list(
    # --> int, get active nr of active rows
    nrow = function() {
      if (is.na(private$cache$nrow))
        private$cache$nrow = private$rows[status == "active", .N]
      private$cache$nrow
    },

    # --> int, get active nr of active cols
    ncol = function() {
      length(private$cols)
    },

    # get or set active cols
    active.cols = function(col.names) {
      if (missing(col.names)) {
        return(private$cols)
      } else {
        private$cols = assertCharacter(col.names, any.missing = FALSE, min.chars = 1L)
        invisible(self)
      }
    },

    # get or set active rows
    active.rows = function(row.names) {
      if (missing(row.names)) {
        private$rows[status == "active"][[self$rowid.col]]
      } else {
        assertSubset(row.names, private$rows[[self$rowid.col]])
        private$rows[.(row.names), status := "active"]
        private$rows[!.(row.names), status := "inactive"]
        private$cache$nrow = NA_integer_
        invisible(self)
      }
    }
  ),

  private = list(
    # data slot, either dplyr or datatable
    data = NULL,
    # [dt], cols = (..id, status), (???, logical)
    rows = NULL,
    # [charvec], active cols
    cols = NULL,
    # [list] local cache
    cache = list(nrow = NA_integer_),

    translateRowIds = function(i = NULL, ids = NULL, active = TRUE) {
      switch(is.null(i) + 2L * is.null(ids) + 1L,
        { # 1: i != NULL, ids != NULL
          stop("Cannot filter backend data with index and ids simultaneously")
        },
        { # 2: i == NULL, ids != NULL
          assertAtomicVector(ids, any.missing = FALSE)
          x = private$rows
          tab = (if (active) x[status == "active"] else x)[.(ids), self$rowid.col, on = self$rowid.col, with = FALSE, nomatch = 0L]
          if (nrow(tab) != length(ids))
            gstop("Invalid ids requested from backend. Expected {length(ids)} rows, query resulted in {nrow(tab)} rows.")
          return(tab)
        },
        { # 3: i != NULL, ids == NULL
          assertIntegerish(i, lower = 0L, upper = self$nrow, any.missing = FALSE)
          x = private$rows
          tab = (if (active) x[status == "active"] else x)[i, self$rowid.col, with = FALSE]
          if (nrow(tab) != length(i))
            gstop("Invalid ids requested from backend. Expected {length(i)} rows, query resulted in {nrow(tab)} rows.")
          return(tab)
        },
        { # 4: i == NULL, ids == NULL
          if (active)
            return(private$rows[status == "active", self$rowid.col, with = FALSE])
          return(private$rows[, self$rowid.col, with = FALSE])
        }
      )
    },

    translateCols = function(cols, active = TRUE) {
      all.cols = if (active) private$cols else self$all.cols
      if (is.null(cols))
        return(all.cols)
      assertCharacter(cols, any.missing = FALSE)

      found = cols %chin% all.cols
      if (!all(found))
        gstop("Invalid columns requested from backend: {stri_peek(cols[!found])}")
      return(cols)
    },

    setRowsTable = function(ids) {
      private$rows = data.table(
        ..id = ids,
        status = factor(rep("active", length(ids)), levels = c("active", "inactive")),
        key = "..id"
      )
      setnames(private$rows, "..id", self$rowid.col)
      private$cache$nrow = length(ids)
    },

    deep_clone = function(name, value) {
      if (name == "rows") copy(value) else value
    }
  )
)
