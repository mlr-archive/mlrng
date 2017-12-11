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

    get = function(rows = NULL, cols = NULL, include.rowid.col = FALSE) {
      rowintersect = function(x, y, key) {
        # y is data.table, x is atomic vector
        if (is.null(x))
          return(y[[1L]])
        if (is.null(y))
          return(x)
        y[list(x), nomatch = 0L, on = key][[1L]]
      }

      colintersect = function(x, y) {
        if (is.null(x))
          return(y)
        if (is.null(y))
          return(x)
        unique(y[chmatch(x, y, 0L)])
      }

      if (!is.null(rows))
        assertAtomicVector(rows)
      assertSubset(cols, colnames(self$internal.data))
      selected.rows = rowintersect(rows, self$view.rows, self$rowid.col)
      selected.cols = colintersect(cols, self$view.cols)

      data = switch(is.null(selected.rows) + 2L * is.null(selected.cols) + 1L,
        self$internal.data[list(selected.rows), c(self$rowid.col, selected.cols), with = FALSE, on = self$rowid.col, nomatch = 0L],
        self$internal.data[, c(self$rowid.col, selected.cols), with = FALSE],
        self$internal.data[list(selected.rows), on = self$rowid.col, nomatch = 0L],
        copy(self$internal.data)
      )

      if (!is.null(rows) && nrow(data) != length(rows))
        stop("Invalid row ids provided")
      if (!include.rowid.col)
        data[[self$rowid.col]] = NULL

      return(private$transform(data))
    },

    subset = function(rows = NULL, cols = NULL) {
      # FIXME: reset; check stuff
      assertSubset(cols, colnames(self$internal.data))
      assertAtomicVector(rows, any.missing = FALSE)

      self$view.rows = data.table(..id = rows, key = "..id")
      setnames(self$view.rows, "..id", self$rowid.col)
      self$view.cols = setdiff(cols, self$rowid.col)
      invisible(self)
    },

    distinct = function(col) {
      assertChoice(col, self$colnames)
      x = self$get(cols = col)[[1L]]
      if (is.factor(x))
        return(as.character(unique(x)))
      return(unique(x))
    },

    head = function(n = 6L) {
      private$transform(self$internal.data[head(self$rownames, n), self$colnames, with = FALSE])
    }
  ),

  active = list(
    data = function(newdata) {
      if (missing(newdata))
        return(private$transform(self$get()))
      assertDataTable(newdata)
      assertNames(names(newdata), must.include = self$rowid.col)
      self$internal.data = as.data.table(newdata)
      setkeyv(self$internal.data, self$rowid.col)
    },

    colnames = function() {
      if (!is.null(self$view.cols))
        return(self$view.cols)
      return(setdiff(colnames(self$internal.data), self$rowid.col))
    },

    rownames = function() {
      if (!is.null(self$view.rows))
        return(self$view.rows[[1L]])
      return(self$internal.data[[self$rowid.col]])
    },

    nrow = function() {
      if (!is.null(self$view.rows))
        return(nrow(self$view.rows))
      return(nrow(self$internal.data))
    },

    ncol = function() {
      if (!is.null(self$view.cols))
        return(length(self$view.cols))
      return(ncol(self$internal.data) - 1L)
    },

    missing.values = function() {
      return(viapply(self$data, function(x) sum(is.na(x))))
    }
  )
)
