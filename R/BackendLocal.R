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
      setkeyv(self$internal.data, rowid.col)
    },

    get = function(rows, cols) {
      assertAtomicVector(rows)
      assertSubset(cols, colnames(self$internal.data))

      data = self$internal.data[list(rows), union(self$rowid.col, cols), with = FALSE, on = self$rowid.col, nomatch = 0L]

      if (self$rowid.col %nin% cols)
        data[[self$rowid.col]] = NULL
      if (nrow(data) != length(rows))
        stop("Invalid row ids provided")
      if (ncol(data) != length(cols))
        stop("Invalid col ids provided")

      return(data)
    },

    distinct = function(col) {
      assertChoice(col, colnames(self$internal.data))
      x = unique(self$internal.data[, col, with = FALSE][[1L]])
      if (is.factor(x)) as.character(x) else x
    },

    missing.values = function(col) {
      assertChoice(col, colnames(self$internal.data))
      sum(is.na(self$internal.data[[col]]))
    },

    head = function(n = 6L) {
      head(self$internal.data, n)
    }
  ),

  active = list(
    data = function(newdata) {
      if (missing(newdata))
        return(copy(self$internal.data))
      assertDataTable(newdata)
      assertNames(names(newdata), must.include = self$rowid.col)
      self$internal.data = as.data.table(newdata)
      setkeyv(self$internal.data, self$rowid.col)
    },

    colnames = function() {
      colnames(self$internal.data)
    },

    rownames = function() {
      self$internal.data[[self$rowid.col]]
    },

    nrow = function() {
      nrow(self$internal.data)
    },

    ncol = function() {
      ncol(self$internal.data)
    }
  )
)
