DataBackend = R6Class("DataBackend",
  public = list(
    id.col = NULL, # [string], col in data that stores row ids

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
        private$rows[status == "active"][[self$id.col]]
      } else {
        assertSubset(row.names, private$rows[[self$id.col]])
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

    translateIds = function(ids) {
      if (is.null(ids)) {
        private$rows[status == "active", self$id.col, with = FALSE]
      } else {
        assertAtomicVector(ids, any.missing = FALSE)
        private$rows[status == "active"][.(ids), self$id.col, on = self$id.col, with = FALSE, nomatch = 0L]
      }
    },

    translateCols = function(cols) {
      if (is.null(cols)) {
        private$cols
      } else {
        assertCharacter(cols, any.missing = FALSE)
        assertSubset(cols, private$cols)
      }
    }
  )
)
