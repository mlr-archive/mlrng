Connection = R6Class("Connection",
  public = list(
    connector = NULL,
    pars = NULL,
    table = NULL,
    rowid.col = NULL,
    internal.con = NULL
  ),

  active = list(
    con = function() {
      if (is.null(self$internal.con) || !DBI::dbIsValid(self$internal.con$con))
        self$internal.con = do.call(self$connector, self$pars)
      self$internal.con
    },

    tbl = function() {
      dplyr::tbl(self$table, src = self$con)
    }
  )
)

ConnectionCustom = R6Class("ConnectionCustom",
  inherit = Connection,
  public = list(
    initialize = function(connector, table, rowid.col = NULL, pars = list()) {
      self$connector = match.fun(connector)
      self$table = assertString(table)
      self$rowid.col = assertString(rowid.col)
      self$pars = assertList(pars, names = "unique")
    }
  )
)

ConnectionMem = R6Class("ConnectionMem",
  inherit = Connection,
  public = list(
    initialize = function(name, data, rowid.col = NULL, ...) {
      self$connector = dplyr::src_sqlite
      self$table = assertString(name)
      assertDataFrame(data)

      if (is.null(rowid.col)) {
        rowid.col = "rowid"
        data[["rowid"]] = seq_len(nrow(data))
      } else {
        assertSubset(rowid.col, names(data))
      }
      self$rowid.col = rowid.col

      pars = list(...)
      pars[c("create", "path")] = list(TRUE, ":memory:")
      self$internal.con = do.call(dplyr::src_sqlite, pars)
      dplyr::copy_to(dest = self$internal.con, df = data, name = name, overwrite = TRUE, temporary = FALSE, unique_indexes = list(rowid.col))
      self$pars = pars
    }
  )
)

if (FALSE) {
  con = ConnectionMem$new("iris", iris)
  con$tbl
}
