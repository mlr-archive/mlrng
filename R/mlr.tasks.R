#' @include TaskClassif.R
#' @include TaskRegr.R
#' @include Dictionary.R
DictionaryTasks = R6Class("DictionaryTasks", inherit = Dictionary,
  public = list(
    initialize = function() {
      super$initialize("Task")
    },

    summary = function(ids = NULL) {
      extractSummary(self, ids,
        function(obj) list(task.type = obj$task.type, nrow = obj$nrow, ncol = obj$ncol))
    }
  )
)

#' @title Dictionary of defined Tasks
#' @docType class
#' @format \code{\link{R6Class}} object
#'
#' @description
#' \code{Tasks} is a \code{\link{Dictionary}} used to manage tasks.
#'
#' @export
#' @examples
#' # List task ids:
#' mlr.tasks$ids
#'
#' # Get a briew summary:
#' mlr.tasks$summary()
#'
#' # Retrieve a specific task:
#' mlr.tasks$get("iris")
#'
#' # Add a new task, based on a subset of iris:
#' data = iris
#' data$Species = ifelse(data$Species == "setosa", "1", "0")
#' task = TaskClassif$new("iris.binary", data = data, target = "Species")
#' task$nclasses
#' mlr.tasks$add(task)
#' mlr.tasks$summary()
#' mlr.tasks$remove("iris.binary")
mlr.tasks = DictionaryTasks$new()

getExampleTaskBackend = function(name, rowid.col = "rowid") {
  fn = system.file("extdata", sprintf("%s.sqlite", name), package = "mlrng", mustWork = TRUE)
  BackendDBI$new(data = list(drv = RSQLite::SQLite(), dbname = fn, flags = RSQLite::SQLITE_RO), rowid.col, name)
}

mlr.tasks$add(
  TaskClassif$new(id = "iris", getExampleTaskBackend("iris"), target = "Species")
)

mlr.tasks$add(
  TaskRegr$new(id = "bh", getExampleTaskBackend("bh"), target = "medv")
)

mlr.tasks$add(
   TaskClassif$new(id = "sonar", getExampleTaskBackend("sonar"), target = "Class", positive = "M")
)

mlr.tasks$add(
  TaskClassif$new(id = "pima", getExampleTaskBackend("pima"), target = "diabetes", positive = "pos")
)

mlr.tasks$add(
  TaskClassif$new(id = "spam", getExampleTaskBackend("spam"), target = "type", positive = "spam")
)

mlr.tasks$add(
  TaskClassif$new(id = "zoo", getExampleTaskBackend("zoo", "animal"), target = "type")
)
