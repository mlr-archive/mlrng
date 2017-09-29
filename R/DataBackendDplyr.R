#' @include DataBackend.R
DataBackendDplyr = R6Class("DataBackendDplyr",
  inherit = DataBackend,
  public = list(
    rowid.col = NULL,

    initialize = function(data, rowid.col = NULL) {
      requireNS(c("dplyr", "lazyeval"))

      private$data = assertClass(data, "tbl_sql")
      cn = colnames(data)
      self$rowid.col = assertSubset(rowid.col, cn)
      ids = dplyr::collect(dplyr::select(data, rowid.col))[[1L]]
      if (anyDuplicated(ids))
        stop("Duplicated ids in rowid.col")
      private$cols = setdiff(cn, rowid.col)
      private$setRowsTable(ids)
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
    nas = function() {
      if (is.null(private$cache$nas)) {
        tab = dplyr::summarize_at(private$data, private$cols, dplyr::funs(sum(is.na(.))))
        private$cache$nas = unlist(as.list(dplyr::collect(tab)))
      }
      private$cache$nas
    },
    all.cols = function() colnames(private$data),
    all.rows = function() dplyr::collect(dplyr::select(private$data, self$rowid.col))[[1L]]
  )
)
