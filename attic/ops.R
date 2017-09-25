# DataBackend

#' @export
`[.DataBackend` = function(x, i, j, ...) {
  if (missing(i)) i = NULL
  if (missing(j)) j = NULL
  x$get(ids = i, cols = j)
}

#' @export
`[[.DataBackend` = function(x, i, ...) {
  assertString(i)
  x$get(cols = i)[[1L]]
}

#' @export
is.Task = function(x) {
  inherits(x, "Task")
}

#' @export
`[.Task` = function(x, i, j, ...) {
  i = if (missing(i)) NULL else x$backend$ids(i)
  if (missing(j)) j = NULL
  x$backend$get(ids = i, cols = j)
}

#' @export
`[[.Task` = function(x, i, ...) {
  assertString(i)
  x$backend$get(ids = NULL, cols = i)[[1L]]
}

#' @export
as.data.table.Task = function(x, keep.rownames = FALSE, ...) {
  x$backend$get()
}

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

#' @export
is.Resampling = function(x) {
  inherits(x, "Resampling")
}

#' @export
length.Resampling = function(x) {
  x$iters
}

#' @export
`[[.Resampling` = function(x, i, ...) {
  if (is.null(x$instance))
    stop("Resampling has not been instantiated yet")
  assertInt(i, lower = 1L, upper = x$iters)
  x$instance[[i]]
}

