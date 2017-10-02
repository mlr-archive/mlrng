#' @include Dictionaries.R

getExampleConnection = function(name, rowid.col = "rowid") {
  fn = system.file("extdata", sprintf("%s.sqlite", name), package = "mlrng", mustWork = TRUE)
  ConnectionCustom$new(dplyr::src_sqlite, name, rowid.col, list(path = fn))
}

mlr.tasks$add(
  LazyElement$new("iris", function() TaskClassif$new(id = "iris", getExampleConnection("iris"), target = "Species"))
)

mlr.tasks$add(
  LazyElement$new("bh", function() TaskRegr$new(id = "bh", getExampleConnection("bh"), target = "medv"))
)

mlr.tasks$add(
  LazyElement$new("sonar", function() TaskClassif$new(id = "sonar", getExampleConnection("sonar"), target = "Class", positive = "M"))
)

mlr.tasks$add(
  LazyElement$new("pima", function() TaskClassif$new(id = "pima", getExampleConnection("pima"), target = "diabetes", positive = "pos"))
)

mlr.tasks$add(
  LazyElement$new("spam", function() TaskClassif$new(id = "spam", getExampleConnection("spam"), target = "type", positive = "spam"))
)

mlr.tasks$add(
  LazyElement$new("zoo", function() {
    TaskClassif$new(id = "zoo",getExampleConnection("zoo", "animal"), target = "type")
  })
)
