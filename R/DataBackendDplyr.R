#' @include DataBackend.R
DataBackendDplyr = R6Class("DataBackendDplyr",
  inherit = DataBackend,
  public = list(
    rows = NULL,
    cols = NULL,
    rowid.col = NULL,

    initialize = function(data, rowid.col = NULL) {
      requireNS(c("dplyr", "lazyeval"))

      assertClass(data, "tbl_sql")
      cn = colnames(data)
      assertSubset(rowid.col, cn)
      ids = dplyr::collect(dplyr::select(data, rowid.col))[[1L]]
      if (anyDuplicated(ids))
        stop("Duplicated ids in rowid.col")

      self$rowid.col = rowid.col
      private$cols = setdiff(cn, rowid.col)
      private$rows = data.table(
        ..id = ids,
        status = factor(rep("active", length(ids)), levels = c("active", "inactive")),
        key = "..id")
      setnames(private$rows, "..id", rowid.col)
      private$data = data
    },

    get = function(i = NULL, ids = NULL, cols = NULL, active = TRUE) {
      ids = private$translateRowIds(i, ids, active)
      cols = private$translateCols(cols, active)

      f = lazyeval::interp("id %in% ids", id = as.name(self$rowid.col), ids = ids[[self$rowid.col]])
      tab = dplyr::filter_(private$data, f)
      tab = dplyr::select(tab, dplyr::one_of(cols))
      tab = dplyr::collect(tab)
      tab = dplyr::mutate_if(tab, is.character, as.factor)
      setDT(tab)[]
    }
  ),

  active = list(
    types = function() vcapply(self$get(i = 1L), class),
    all.cols = function() colnames(private$data),
    all.rows = function() dplyr::collect(dplyr::select(private$data, self$rowid.col))[[1L]]
  ),

  private = list(
    deep_clone = function(name, value) {
      if (name == "rows") copy(value) else value
    }
  )
)
