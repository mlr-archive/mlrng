#' @include DataBackend.R
DataBackendDataTable = R6Class("DataBackendDataTable",
  inherit = DataBackend,
  public = list(
    initialize = function(data, cols = NULL, id.col = NULL) {
      data = as.data.table(data)

      if (is.null(cols)) {
        cols = names(data)
      }

      if (is.null(id.col)) {
        id.col = "..id"
        data[[id.col]] = seq_len(nrow(data))
      } else {
        assertChoice(id.col, names(data))
        if (anyDuplicated(data[[id.col]]))
          stop("Duplicated ids in ID column")
        cols = setdiff(cols, id.col)
      }

      assertDataTable(data[1L, cols, with = FALSE], types = c("logical", "numeric", "factor"))
      setkeyv(data, id.col)
      self$cols = setdiff(cols, id.col)
      self$id.col = id.col

      self$rows = data.table(
        ..id = data[[self$id.col]],
        status = factor(rep("active", nrow(data)), levels = c("active", "inactive")),
        key = "..id"
      )
      setnames(self$rows, "..id", self$id.col)
      private$data = data
    },

    get = function(ids = NULL, cols = NULL) {
      ids = private$translateIds(ids)
      cols = private$translateCols(cols)
      private$data[.(ids), cols, with = FALSE, on = self$id.col]
    }
  ),


  active = list(
    # --> charvec, get datatypes of active cols
    types = function() vcapply(private$data[, self$cols, with = FALSE], class)
  ),

  private = list(
    deep_clone = function(name, value) {
      if (name == "rows") copy(value) else value
    }
  )
)

