#' @include Dictionaries.R
getExampleView = function(name, rowid.col = "rowid") {
  fn = system.file("extdata", sprintf("%s.sqlite", name), package = "mlrng", mustWork = TRUE)
  View$new(list(drv = RSQLite::SQLite(), dbname = fn, flags = RSQLite::SQLITE_RO), name, rowid.col)
}

mlr.tasks$add(
  LazyElement$new("iris", function() TaskClassif$new(id = "iris", getExampleView("iris"), target = "Species"))
)

mlr.tasks$add(
  LazyElement$new("bh", function() TaskRegr$new(id = "bh", getExampleView("bh"), target = "medv"))
)

mlr.tasks$add(
  LazyElement$new("sonar", function() TaskClassif$new(id = "sonar", getExampleView("sonar"), target = "Class", positive = "M"))
)

mlr.tasks$add(
  LazyElement$new("pima", function() TaskClassif$new(id = "pima", getExampleView("pima"), target = "diabetes", positive = "pos"))
)

mlr.tasks$add(
  LazyElement$new("spam", function() TaskClassif$new(id = "spam", getExampleView("spam"), target = "type", positive = "spam"))
)

mlr.tasks$add(
  LazyElement$new("zoo", function() {
    TaskClassif$new(id = "zoo", getExampleView("zoo", "animal"), target = "type")
  })
)
