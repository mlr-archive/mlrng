#' @include DataBackend.R
DataBackendDataTable = R6Class("DataBackendDataTable",
  inherit = DataBackend,
  public = list(
    initialize = function(data, cols = NULL, rowid.col = NULL) {
      data = as.data.table(data)

      if (is.null(cols)) {
        cols = names(data)
      }

      if (is.null(rowid.col)) {
        self$rowid.col = "..id"
        data[["..id"]] = seq_len(nrow(data))
        private$cols = cols
      } else {
        self$rowid.col = assertChoice(rowid.col, names(data))
        if (anyDuplicated(data[[rowid.col]]))
          stop("Duplicated ids in rowid.col")
        private$cols = setdiff(cols, rowid.col)
      }

      private$rows = getRowsTable(data[[self$rowid.col]], self$rowid.col)

      assertDataTable(data[1L, private$cols, with = FALSE], types = c("logical", "numeric", "factor"))
      private$data = setkeyv(data, self$rowid.col)
    },

    get = function(i = NULL, ids = NULL, cols = NULL, active = TRUE) {
      ids = private$translateRowIds(i, ids, active)
      cols = private$translateCols(cols, active)
      private$data[.(ids), cols, with = FALSE, on = self$rowid.col]
    }
  ),

  active = list(
    # --> charvec, get datatypes of active cols
    types = function() vcapply(private$data[, private$cols, with = FALSE], class),
    all.rows = function() private$data[[self$rowid.col]],
    all.cols = function() colnames(private$data)
  )
)
