task = mlr.tasks$get("iris")
task = task$clone(deep = TRUE)

newdata = data.table(rowid = 5L, Sepal.Length = 99)

updateData = function(task, newdata) {
  rowid.col = task$backend$rowid.col
  if (!inherits(task$backend, "BackendLocal")) {
    data = task$backend$get(rows = task$rows("training"), cols = task$cols(c("primary.id", "feature", "target")))
    task$backend = BackendLocal$new(data, rowid.col = rowid.col)
  }

  assertDataTable(newdata)
  assertNames(names(newdata), must.include = rowid.col)

  cn = setdiff(names(newdata), rowid.col)
  expr = parse(text = stri_join("`:=`(", stri_flatten(sprintf("%1$s=i.%1$s", cn), ","), ")"))
  task$backend$internal.data[newdata, eval(expr), on = rowid.col]
}
