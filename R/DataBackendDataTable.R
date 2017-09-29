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
        rowid.col = "..id"
        data[[rowid.col]] = seq_len(nrow(data))
      } else {
        assertChoice(rowid.col, names(data))
        if (anyDuplicated(data[[rowid.col]]))
          stop("Duplicated ids in rowid.column")
        cols = setdiff(cols, rowid.col)
      }

      assertDataTable(data[1L, cols, with = FALSE], types = c("logical", "numeric", "factor"))
      setkeyv(data, rowid.col)
      private$cols = setdiff(cols, rowid.col)
      self$rowid.col = rowid.col

      private$rows = data.table(
        ..id = data[[self$rowid.col]],
        status = factor(rep("active", nrow(data)), levels = c("active", "inactive")),
        key = "..id"
      )
      setnames(private$rows, "..id", self$rowid.col)
      private$data = data
    },

    get = function(ids = NULL, cols = NULL) {
      ids = private$translateIds(ids)
      cols = private$translateCols(cols)
      private$data[.(ids), cols, with = FALSE, on = self$rowid.col]
    }
  ),


  active = list(
    # --> charvec, get datatypes of active cols
    types = function() vcapply(private$data[, private$cols, with = FALSE], class),
    all.rows = function() private$data[[self$rowid.col]],
    all.cols = function() colnames(private$data)
  ),

  private = list(
    deep_clone = function(name, value) {
      if (name == "rows") copy(value) else value
    }
  )
)

