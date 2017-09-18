DataBackend = R6Class("DataBackend",
  public = list(
    data = NULL,
    rows = NULL,
    cols = NA_character_,
    id.col = NA_character_,

    # translate numeric/logical index to ids
    ids = function(i = NULL) {
      if (is.null(i))
        self$rows[status == "active", self$id.col, with = FALSE][[1L]]
      else
        self$rows[status == "active", self$id.col, with = FALSE][i][[1L]]
    },

    select = function(features) {
      assertCharacter(features, any.missing = FALSE)
      self$cols = intersect(self$cols, features)
    },

    drop = function(features) {
      assertCharacter(features, any.missing = FALSE)
      self$cols = setdiff(self$cols, features)
    },

    slice = function(i) {
      i = self$rows[status == "active"][i, self$id.col, nomatch = 0L, with = FALSE]
      self$rows[!i, status := "inactive"]
      setkeyv(self$rows, self$id.col)
    },

    subsample = function(n = NULL, ratio = NULL) {
      if (is.null(n) + is.null(ratio) != 1L)
        stop("Either 'n' or 'ratio' must be not NULL")
      nr = self$nrow
      if (!is.null(n)) {
        self$slice(sample(seq_len(nr), min(n, nr)))
      } else {
        self$slice(sample(seq_len(nr), ratio * nr))
      }
    }
  ),

  active = list(
    nrow = function() self$rows[status == "active", .N],
    ncol = function() length(self$cols)
  ),

  private = list(
    translate_ids = function(ids) {
      if (is.null(ids)) {
        self$rows[status == "active", self$id.col, with = FALSE]
      } else {
        self$rows[status == "active"][.(ids), self$id.col, on = self$id.col, with = FALSE, nomatch = 0L]
      }
    },

    translate_cols = function(cols) {
      if (is.null(cols)) {
        self$cols
      } else {
        assertCharacter(cols, any.missing = FALSE)
        assertSubset(cols, self$cols)
      }
    }
  )
)

#' @export
`[.DataBackend` = function(x, i, j, ...) {
  if (missing(i)) i = NULL
  if (missing(j)) j = NULL
  x$get(ids = i, cols = j)
}

#' @export
`[[.DataBackend` = function(x, i, ...) {
  assertString(i)
  x$get(cols = i)[[1L]]
}
