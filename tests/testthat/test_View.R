context("View")

fn = tempfile("iris_", fileext = ".sqlite")
con = DBI::dbConnect(RSQLite::SQLite(), dbname = fn)
data = iris
data$id = sprintf("r%03i", seq_len(nrow(iris)))
DBI::dbWriteTable(con, "iris", data)
DBI::dbDisconnect(con)

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
  expect_set_equal(names(v$pars), c("drv", "dbname", "flags"))
  expect_file_exists(v$pars$dbname, access = "r")
  expect_numeric(file.size(v$pars$dbname), min = 1)
  expect_true(DBI::dbExistsTable(v$con, v$name))
  expect_set_equal(v$distinct("Species"), levels(iris$Species))
})


test_that("View: cache gets invalidated", {
  populate_cache = function(v) {
    v$nrow
    v$ncol
    v$types
    v$distinct("Species")
    v$na.cols
  }

  v = asView(data = iris)
  expect_set_equal(ls(private(v)$cache), character(0))

  populate_cache(v)
  expect_set_equal(ls(private(v)$cache), c("nrow", "types", "distinct", "na.cols"))
  expect_set_equal(private(v)$cache$distinct$Species, levels(iris$Species))

  v$active.rows = 1:40
  expect_set_equal(ls(private(v)$cache), c("nrow", "types"))
  expect_identical(private(v)$cache$nrow, 40L)

  populate_cache(v)
  v$active.cols = "Species"
  expect_set_equal(ls(private(v)$cache), c("distinct", "nrow"))
})
