#' @include Backend.R
BackendLocal = R6Class("BackendLocal", inherit = Backend,
  public = list(
    internal.data = NULL,

    initialize = function(data, rowid.col = NULL) {
      assertDataFrame(data)
      self$internal.data = as.data.table(data)

      if (is.null(rowid.col)) {
        self$internal.data[["..id"]] = seq_len(nrow(data))
        self$rowid.col = "..id"
      } else {
        assertNames(names(data), must.include = rowid.col)
        self$rowid.col = rowid.col
      }
    },

    get = function(rows = NULL, cols = NULL, include.rowid.col = FALSE) {
      assertSubset(cols, colnames(self$internal.data))
      data = switch(is.null(rows) + 2L * is.null(cols) + 1L,
        self$internal.data[list(rows), c(self$rowid.col, cols), with = FALSE, on = self$rowid.col, nomatch = 0L],
        self$internal.data[, c(self$rowid.col, cols), with = FALSE],
        self$internal.data[list(rows), on = self$rowid.col, nomatch = 0L],
        copy(self$internal.data)
      )
      if (!is.null(rows) && nrow(data) != length(rows))
        stop("Invalid row ids provided")
      if (!include.rowid.col)
        data[[self$rowid.col]] = NULL
      return(private$mutate(data))
    },

    subset = function(rows = NULL, cols = NULL) {
      self$internal.data = self$get(rows, cols, include.rowid.col = TRUE)
      invisible(self)
    },

    distinct = function(col) {
      assertChoice(col, self$colnames)
      x = self$internal.data[[col]]
      if (is.factor(x)) levels(x) else unique(x)
    },

    head = function(n = 6L) {
      private$mutate(head(self$internal.data[, !(self$rowid.col), with = FALSE], n))
    }
  ),

  active = list(
    data = function(newdata) {
      if (missing(newdata)) {
        return(private$mutate(self$internal.data[, !(self$rowid.col), with = FALSE]))
      }
      assertDataTable(newdata)
      assertNames(names(newdata), must.include = self$rowid.col)
      self$internal.data = as.data.table(newdata)
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
      vcapply(self$head(1L), class)
    },

    missing.values = function() {
      # FIXME: this copies the data
      return(viapply(self$data, anyMissing))
    }
  )
)
