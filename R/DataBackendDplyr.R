#' @include DataBackend.R
DataBackendDplyr = R6Class("DataBackendDplyr",
  inherit = DataBackend,
  public = list(
    data = NULL,
    rows = NULL,
    cols = NULL,
    id.col = NULL,

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

    get = function(ids = NULL, cols = NULL) {
      ids = private$translateIds(ids)
      cols = private$translateCols(cols)

      f = lazyeval::interp("id %in% ids", id = as.name(self$id.col), ids = ids[[self$id.col]])
      tab = dplyr::filter_(self$data, f)
      tab = dplyr::select(tab, dplyr::one_of(cols))
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
