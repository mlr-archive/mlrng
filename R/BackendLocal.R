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

    get = function(rows = NULL, cols = NULL) {
      if (!is.null(rows))
        assertAtomicVector(rows)
      if (!is.null(cols))
        assertSubset(cols, colnames(self$internal.data))

      data = switch(is.null(rows) + 2L * is.null(cols) + 1L,
        self$internal.data[list(rows), c(self$rowid.col, cols), with = FALSE, on = self$rowid.col, nomatch = 0L],
        self$internal.data[, c(self$rowid.col, cols), with = FALSE],
        self$internal.data[list(rows), on = self$rowid.col, nomatch = 0L],
        copy(self$internal.data)
      )

      if (!is.null(rows) && nrow(data) != length(rows))
        stop("Invalid row ids provided")
      if (!is.null(cols) && self$rowid.col %nin% cols)
        data[[self$rowid.col]] = NULL

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
    # FIXME: rename -> set_data?
    data = function(newdata) {
      if (missing(newdata))
        return(self$get())
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
