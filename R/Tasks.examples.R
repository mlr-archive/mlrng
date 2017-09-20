#' @include Tasks.R
getDataSet = function(name, pkg = "mlrng") {
  if (length(find.package(pkg, quiet = TRUE)) == 0L)
    gstop("Please install package '{pkg}' for data set '{name}'")
  ee = new.env(hash = FALSE)
  data(list = name, package = pkg, envir = ee)
  ee[[name]]
}

addExampleTask = function(taskcl, id, dsname, package, target, pos = NULL) {
  Tasks$add(LazyElement$new(id, get = function() {
    d = getDataSet(dsname, package)
    dbe = DataBackendDataTable$new(data = d)
    args = list(id = id, backend = dbe, target = target)
    args$positive = pos # only add when not NULL
    do.call(taskcl$new, args)
  }))
}

addExampleTask(TaskClassif, "iris", "iris", "datasets", "Species")
addExampleTask(TaskClassif, "sonar", "Sonar", "mlbench", "Class", "M")
addExampleTask(TaskClassif, "pima", "PimaIndiansDiabetes2", "mlbench", "diabetes", "pos")
addExampleTask(TaskClassif, "spam", "spam", "kernlab", "type", "spam")
addExampleTask(TaskRegr, "bh", "BostonHousing", "mlbench", "medv")


