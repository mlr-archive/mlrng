requireNamespace("rprojroot")
requireNamespace("DBI")
path = file.path(rprojroot::find_package_root_file(), "inst", "extdata")

getDataSet = function(name, pkg = "mlrng") {
  if (length(find.package(pkg, quiet = TRUE)) == 0L)
    gstop("Please install package '{pkg}' for data set '{name}'")
  ee = new.env(hash = FALSE)
  data(list = name, package = pkg, envir = ee)
  ee[[name]]
}

writeSQL = function(name, data, primary) {
  con = DBI::dbConnect(RSQLite::SQLite(), file.path(path, sprintf("%s.sqlite", name)), create = TRUE)
  DBI::dbWriteTable(con, name, data, overwrite = TRUE)
  res = DBI::dbSendQuery(con, sprintf("CREATE UNIQUE INDEX key ON %s (%s)", name, primary))
  DBI::dbClearResult(res)
  DBI::dbDisconnect(con)
}

# iris
data = getDataSet("iris", "datasets")
data$rowid = seq_len(nrow(data))
writeSQL("iris", data, "rowid")

# boston housing
data = getDataSet("BostonHousing", "mlbench")
data$rowid = seq_len(nrow(data))
writeSQL("bh", data, "rowid")

# sonar
data = getDataSet("Sonar", "mlbench")
data$rowid = seq_len(nrow(data))
writeSQL("sonar", data, "rowid")

# pima
data = getDataSet("PimaIndiansDiabetes2", "mlbench")
data$rowid = seq_len(nrow(data))
writeSQL("pima", data, "rowid")

# spam
data = getDataSet("spam", "kernlab")
data$rowid = seq_len(nrow(data))
writeSQL("spam", data, "rowid")

# zoo
data = getDataSet("Zoo", "mlbench")
data$animal = factor(rownames(data))
writeSQL("zoo", data, "animal")
