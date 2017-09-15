#' @include Dictionary.R
#' @include Task.R
getDataSet = function(name, pkg = "mlrng") {
  if (length(find.package(pkg, quiet = TRUE)) == 0L)
    gstop("Please install package '{pkg}' for data set '{name}'")
  ee = new.env(hash = FALSE)
  data(list = name, package = pkg, envir = ee)
  ee[[name]]
}

Tasks$add(
  LazyElement$new("iris", function() ClassifTask$new(id = "iris", getDataSet("iris", "datasets"), target = "Species"))
)

Tasks$add(
  LazyElement$new("bh", function() RegrTask$new(id = "bh", getDataSet("BostonHousing", "mlbench"), target = "medv"))
)

Tasks$add(
  LazyElement$new("sonar", function() ClassifTask$new(id = "sonar", getDataSet("Sonar", "mlbench"), target = "Class", positive = "M"))
)

Tasks$add(
  LazyElement$new("pima", function() ClassifTask$new(id = "pima", getDataSet("PimaIndiansDiabetes2", "mlbench"), target = "diabetes", positive = "pos"))
)

Tasks$add(
  LazyElement$new("spam", function() ClassifTask$new(id = "spam", getDataSet("spam", "kernlab"), target = "type", positive = "spam"))
)

Tasks$add(
  LazyElement$new("breastcancer", function() {
    data = getDataSet("BreastCancer", "mlbench")
    i = vlapply(data, is.ordered)
    data[i] = lapply(data[i], as.integer)
    cols = setdiff(names(data), "Id")
    task = ClassifTask$new(id = "BreastCancer", data = data, target = "Class", cols = cols, positive = "malignant")
    task$backend$drop("Id")
    task
  })
)
