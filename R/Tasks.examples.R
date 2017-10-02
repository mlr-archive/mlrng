#' @include Tasks.R
getExampleConnection = function(name, rowid.col = "rowid") {
  fn = system.file("extdata", sprintf("%s.sqlite", name), package = "mlrng", mustWork = TRUE)
  ConnectionCustom$new(dplyr::src_sqlite, name, rowid.col, list(path = fn))
}

Tasks$add(
  LazyElement$new("iris", function() TaskClassif$new(id = "iris", getExampleConnection("iris"), target = "Species"))
)

Tasks$add(
  LazyElement$new("bh", function() TaskRegr$new(id = "bh", getExampleConnection("bh"), target = "medv"))
)

Tasks$add(
  LazyElement$new("sonar", function() TaskClassif$new(id = "sonar", getExampleConnection("sonar"), target = "Class", positive = "M"))
)

Tasks$add(
  LazyElement$new("pima", function() TaskClassif$new(id = "pima", getExampleConnection("pima"), target = "diabetes", positive = "pos"))
)

Tasks$add(
  LazyElement$new("spam", function() TaskClassif$new(id = "spam", getExampleConnection("spam"), target = "type", positive = "spam"))
)

Tasks$add(
  LazyElement$new("zoo", function() {
    TaskClassif$new(id = "zoo",getExampleConnection("zoo", "animal"), target = "type")
  })
)
