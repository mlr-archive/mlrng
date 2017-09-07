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

    translate_cols = function(j) {
      if (is.null(j)) {
        self$cols
      } else {
        assertCharacter(j, any.missing = FALSE)
        assertSubset(j, self$cols)
      }
    }
  )
)

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

    get = function(ids = NULL, j = NULL) {
      i = private$translate_ids(ids)
      j = private$translate_cols(j)
      self$data[.(i), j, with = FALSE, on = self$id.col]
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

DplyrBackend = R6Class("DplyrBackend",
  inherit = DataBackend,
  public = list(
    data = NULL,
    rows = NULL,
    cols = NA_character_,
    id.col = NA_character_,

    initialize = function(data, id.col = NULL) {
      requireNS(c("dplyr", "lazyeval"))

      assertClass(data, "tbl_sql")
      cn = colnames(data)
      assertSubset(id.col, cn)
      ids = dplyr::collect(dplyr::select(data, id.col))[[1L]]
      if (anyDuplicated(ids))
        stop("Duplicated ids in ID column")

      self$id.col = id.col
      self$cols = setdiff(cn, id.col)
      self$rows = data.table(
        ..id = ids,
        status = factor(rep("active", length(ids)), levels = c("active", "inactive")),
        key = "..id")
      setnames(self$rows, "..id", id.col)
      self$data = data
    },

    get = function(ids = NULL, j = NULL) {
      ids = private$translate_ids(ids)
      j = private$translate_cols(j)

      f = lazyeval::interp("id %in% ids", id = as.name(self$id.col), ids = ids[[self$id.col]])
      tab = dplyr::filter_(self$data, f)
      tab = dplyr::select(tab, dplyr::one_of(j))
      tab = dplyr::collect(tab)
      tab = dplyr::mutate_if(tab, is.character, as.factor)
      setDT(tab)[]
    }
  ),

  active = list(
    types = function() {
      vcapply(self$get(1L), class)
    }
  ),

  private = list(
    deep_clone = function(name, value) {
      if (name == "rows") copy(value) else value
    }
  )
)

#' @export
`[.DataBackend` = function(x, i, j, ...) {
  if (missing(i)) i = NULL
  if (missing(j)) j = NULL
  x$get(ids = i, j = j)
}

#' @export
`[[.DataBackend` = function(x, i, ...) {
  assertString(i)
  x$get(j = i)[[1L]]
}
