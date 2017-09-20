DataBackend = R6Class("DataBackend",
  public = list(
    data = NULL,   # data slot, either dplyr or datatable
    rows = NULL,   # [dt], cols = (..id, status), (???, logical)
    cols = NULL,   # [charvec], active cols
    id.col = NULL, # [string], col in data that stores row ids

    # translate numeric/logical index to ids
    ids = function(i = NULL) {
      if (is.null(i))
        self$rows[status == "active", self$id.col, with = FALSE][[1L]]
      else
        self$rows[status == "active", self$id.col, with = FALSE][i][[1L]]
    },

    # sets active cols, charvec --> X
    setActiveCols = function(cols) {
      assertSubset(cols, self$cols)
      self$cols = cols
      return(self)
    },

    # sets active cols, charvec --> X
    # cols  also include already inactive cols
    dropActiveCols = function(cols) {
      assertSubset(cols, self$cols)
      self$cols = setdiff(self$cols, cols)
      return(self)
    },

    # sets active rows, intvec --> X
    setActiveRows = function(i) {
      assertInteger(i, lower = 1L, upper = self$nrow, any.missing = FALSE)
      i = self$rows[status == "active"][i, self$id.col, nomatch = 0L, with = FALSE]
      self$rows[!i, status := "inactive"]
      setkeyv(self$rows, self$id.col)
      return(self)
    },

    getCol = function(j) {
      assertChoice(j, self$cols)
      self$get(cols = j)[[1L]]
    },


    subsample = function(n = NULL, ratio = NULL) {
      if (is.null(n) + is.null(ratio) != 1L)
        stop("Either 'n' or 'ratio' must be not NULL")
      nr = self$nrow
      if (!is.null(n)) {
        self$setActiveRows(sample(seq_len(nr), min(n, nr)))
      } else {
        self$setActiveRows(sample(seq_len(nr), ratio * nr))
      }
    }
  ),

  active = list(
    # --> int, get active nr of active rows
    nrow = function() self$rows[status == "active", .N],
    # --> int, get active nr of active cols
    ncol = function() length(self$cols)
  ),

  private = list(
    translateIds = function(ids) {
      if (is.null(ids)) {
        self$rows[status == "active", self$id.col, with = FALSE]
      } else {
        self$rows[status == "active"][.(ids), self$id.col, on = self$id.col, with = FALSE, nomatch = 0L]
      }
    },

    translateCols = function(cols) {
      if (is.null(cols)) {
        self$cols
      } else {
        assertCharacter(cols, any.missing = FALSE)
        assertSubset(cols, self$cols)
      }
    }
  )
)

