#' @include Dictionaries.R
getDataBackend = function(name, rowid.col = "rowid") {
  fn = system.file("extdata", sprintf("%s.sqlite", name), package = "mlrng", mustWork = TRUE)
  BackendDBI$new(data = list(drv = RSQLite::SQLite(), dbname = fn, flags = RSQLite::SQLITE_RO), rowid.col, name)
}

mlr.tasks$add(
  LazyElement$new("iris", function() TaskClassif$new(id = "iris", getDataBackend("iris"), target = "Species"))
)

mlr.tasks$add(
  LazyElement$new("bh", function() TaskRegr$new(id = "bh", getDataBackend("bh"), target = "medv"))
)

mlr.tasks$add(
  LazyElement$new("sonar", function() TaskClassif$new(id = "sonar", getDataBackend("sonar"), target = "Class", positive = "M"))
)

mlr.tasks$add(
  LazyElement$new("pima", function() TaskClassif$new(id = "pima", getDataBackend("pima"), target = "diabetes", positive = "pos"))
)

mlr.tasks$add(
  LazyElement$new("spam", function() TaskClassif$new(id = "spam", getDataBackend("spam"), target = "type", positive = "spam"))
)

mlr.tasks$add(
  LazyElement$new("zoo", function() {
    TaskClassif$new(id = "zoo", getDataBackend("zoo", "animal"), target = "type")
  })
)
