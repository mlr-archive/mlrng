#' @include Tasks.R
getDataSet = function(name, pkg = "mlrng") {
  if (length(find.package(pkg, quiet = TRUE)) == 0L)
    gstop("Please install package '{pkg}' for data set '{name}'")
  ee = new.env(hash = FALSE)
  data(list = name, package = pkg, envir = ee)
  ee[[name]]
}

Tasks$add(
  LazyElement$new("iris", function() TaskClassif$new(id = "iris", getDataSet("iris", "datasets"), target = "Species"))
)

Tasks$add(
  LazyElement$new("bh", function() TaskRegr$new(id = "bh", getDataSet("BostonHousing", "mlbench"), target = "medv"))
)

Tasks$add(
  LazyElement$new("sonar", function() TaskClassif$new(id = "sonar", getDataSet("Sonar", "mlbench"), target = "Class", positive = "M"))
)

Tasks$add(
  LazyElement$new("pima", function() TaskClassif$new(id = "pima", getDataSet("PimaIndiansDiabetes2", "mlbench"), target = "diabetes", positive = "pos"))
)

Tasks$add(
  LazyElement$new("spam", function() TaskClassif$new(id = "spam", getDataSet("spam", "kernlab"), target = "type", positive = "spam"))
)

Tasks$add(
  LazyElement$new("zoo", function() {
    data = getDataSet("Zoo", "mlbench")
    data$animal = factor(rownames(data))
    b = DataBackendDataTable$new(data = data, rowid.col = "animal")
    TaskClassif$new(id = "zoo", data, target = "type")
  })
)

Tasks$add(
  LazyElement$new("breastcancer", function() {
    data = getDataSet("BreastCancer", "mlbench")
    i = vlapply(data, is.ordered)
    data[i] = lapply(data[i], as.integer)
    cols = setdiff(names(data), "Id")
    b = DataBackendDataTable$new(data = data, cols = cols)
    TaskClassif$new(id = "BreastCancer", data = b, target = "Class", positive = "malignant")
  })
)
