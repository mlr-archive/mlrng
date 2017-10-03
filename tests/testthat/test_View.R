context("View")

fn = tempfile("iris_", fileext = ".sqlite")
con = DBI::dbConnect(RSQLite::SQLite(), dbname = fn)
data = iris
data$id = sprintf("r%03i", seq_len(nrow(iris)))
DBI::dbWriteTable(con, "iris", data)
DBI::dbDisconnect(con)

expect_view = function(v, data = NULL, rowid = NULL) {
  expect_r6(v, "View",
    cloneable = TRUE,
    public = c("active.cols", "active.rows", "con", "tbl", "raw.tbl", "name", "pars", "rowid.col"),
    private = c("view.cols", "view.rows"))
  expect_character(v$active.cols, any.missing = FALSE, unique = TRUE)
  if (!is.null(data))
    expect_subset(v$active.cols, names(data))
  expect_atomic_vector(v$active.rows, any.missing = FALSE, unique = TRUE)
  if (!is.null(data) && !is.null(rowid))
    expect_subset(v$active.rows, data[[rowid]])
  expect_true(DBI::dbIsValid(v$con))
  expect_class(v$tbl, c("tbl_sql", "tbl_lazy", "tbl"))
  expect_class(v$raw.tbl, c("tbl_sql", "tbl_lazy", "tbl"))
  expect_string(v$name)
  expect_list(v$pars, names = "unique")
  expect_string(v$rowid.col)
  expect_integer(v$nrow, len = 1L, lower = 0L, upper = nrow(data) %??% Inf)
  expect_integer(v$ncol, len = 1L, lower = 0L, upper = nrow(data) %??% Inf)
  expect_character(v$types, names = "unique")
  expect_subset(v$types, c("logical", "integer", "numeric", "character", "factor"))
  if (!is.null(data))
    expect_subset(names(v$types), names(data))
  expect_tibble(dplyr::collect(head(v$tbl, 1L)), nrow = 1L)
  expect_tibble(dplyr::collect(head(v$raw.tbl, 1L)), nrow = 1L)
}

test_that("View object", {
  pars = list(drv = RSQLite::SQLite(), dbname = fn)
  v = View$new(pars, "iris", rowid.col = "id")
  expect_view(v, data, "id")

  expect_character(v$active.cols, len = 5L)
  expect_set_equal(v$active.cols, names(iris))
  expect_character(v$active.rows, len = 150L)
  expect_set_equal(v$active.rows, data[["id"]])
  expect_equal(v$name, "iris")
  expect_equal(v$pars, pars)
  expect_equal(v$rowid.col, "id")
  expect_identical(v$nrow, 150L)
  expect_identical(v$ncol, 5L)

  expect_tibble(dplyr::collect(v$tbl), nrow = 150L, ncol = 5L)
  expect_tibble(dplyr::collect(v$raw.tbl), nrow = 150L, ncol = 6L)
  v$active.rows = sprintf("r%03i", 41:50)
  v$active.cols = "Sepal.Length"
  expect_tibble(dplyr::collect(v$tbl), nrow = 10L, ncol = 1L)
  expect_tibble(dplyr::collect(v$raw.tbl), nrow = 150L, ncol = 6L)

  expect_error({ v$active.rows = "a" }, "[Ii]nvalid row ids")
  expect_error({ v$active.cols = "a" }, "subset of")

  expect_tibble(dplyr::collect(v$tbl), nrow = 10L, ncol = 1L)
  expect_tibble(dplyr::collect(v$raw.tbl), nrow = 150L, ncol = 6L)
})

test_that("asView", {
  v = asView(data = iris)
  expect_view(v)
  expect_equal(v$rowid.col, "rowid")
  expect_set_equal(names(v$pars), c("drv", "dbname"))
  expect_file_exists(v$pars$dbname, access = "r")
  expect_numeric(file.size(v$pars$dbname), min = 1)
  expect_true(DBI::dbExistsTable(v$con, v$name))
  expect_set_equal(v$distinct("Species"), levels(iris$Species))
})
