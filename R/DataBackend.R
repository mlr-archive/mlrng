DataBackend = R6Class("DataBackend",
  public = list(
    rowid.col = NULL, # [string], col in data that stores row ids

    subsample = function(n = NULL, ratio = NULL) {
      if (is.null(n) + is.null(ratio) != 1L)
        stop("Either 'n' or 'ratio' must be not NULL")
      nr = self$nrow
      if (!is.null(n)) {
        self$active.rows = self$active.rows[sample(seq_len(nr), min(n, nr))]
      } else {
        self$active.rows = self$active.rows[sample(seq_len(nr), ratio * nr)]
      }
      invisible(self)
    }
  ),

  active = list(
    # --> int, get active nr of active rows
    nrow = function() private$rows[status == "active", .N],

    # --> int, get active nr of active cols
    ncol = function() length(private$cols),

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
        invisible(self)
      }
    }
  ),

  private = list(
    data = NULL,   # data slot, either dplyr or datatable
    rows = NULL,   # [dt], cols = (..id, status), (???, logical)
    cols = NULL,   # [charvec], active cols

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
    }
  )
)
