#' @include DataBackend.R
DataTableBackend = R6Class("DataTableBackend",
  inherit = DataBackend,
  public = list(
    initialize = function(data, cols = NULL, id.col = NULL) {
      self$data = as.data.table(data)

      if (is.null(cols)) {
        cols = names(self$data)
      }

      if (is.null(id.col)) {
        id.col = "..id"
        self$data[[id.col]] = seq_len(nrow(data))
      } else {
        assertChoice(id.col, names(self$data))
        if (anyDuplicated(self$data[[id.col]]))
          stop("Duplicated ids in ID column")
        cols = setdiff(cols, id.col)
      }

      assertDataTable(self$data[1L, cols, with = FALSE], types = c("numeric", "factor"))
      setkeyv(self$data, id.col)
      self$cols = setdiff(cols, id.col)
      self$id.col = id.col

      self$rows = data.table(
        ..id = self$data[[self$id.col]],
        status = factor(rep("active", nrow(self$data)), levels = c("active", "inactive")),
        key = "..id"
      )
      setnames(self$rows, "..id", self$id.col)
    },

    get = function(ids = NULL, cols = NULL) {
      ids = private$translate_ids(ids)
      cols = private$translate_cols(cols)
      self$data[.(ids), cols, with = FALSE, on = self$id.col]
    }
  ),

  active = list(
    types = function() vcapply(self$data[, self$cols, with = FALSE], class)
  ),

  private = list(
    deep_clone = function(name, value) {
      if (name == "rows") copy(value) else value
    }
  )
)

