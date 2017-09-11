#' @include Register.R
Tasks = Register$new("Task")

#' @title Base Class for Tasks
#' @format \code{\link{R6Class}} object
#' @usage Task$new(...)
#' @template params-task
#'
#' @description
#' A \code{\link[R6]{R6Class}} to construct tasks.
#' This is the abstract base class, do not use directly!
#'
#' @template fields-task
#' @return [\code{\link{Task}}].
#' @family Tasks
#' @export
Task = R6Class("Task",
  public = list(
    backend = NULL,
    id = NULL,
    hooks = list(),
    initialize = function(data, cols = NULL, id.col = NULL, id = deparse(substitute(data))) {
      self$id = assertString(id, min.chars = 1L)

      if (inherits(data, "DataBackend")) {
        self$backend = copy(data)
      } else if (inherits(data, "tbl_sql")) {
        self$backend = DplyrBackend$new(data, cols = cols, id.col = id.col)
      } else {
        assertDataFrame(data)
        self$backend = DataTableBackend$new(data, cols = cols, id.col = id.col)
      }
    },

    addHook = function(id, fun, ...) {
      hook = list(list(fun = fun, pars = list(...)))
      self$hooks = c(self$hooks, setNames(hook, id))
    }
  ),

  active = list(
    nrow = function() self$backend$nrow,
    ncol = function() self$backend$ncol,
    features = function() self$backend$cols,
    formula = function() reformulate(self$backend$cols)
  ),

  private = list(
    deep_clone = function(name, value) {
      if (name == "backend") value$clone(deep = TRUE) else value
    }
  )
)

#' @title Base Class for Supervised Tasks
#'
#' @usage SupervisedTask$new(...)
#' @template params-task
#' @template params-supervisedtask
#'
#' @description
#' A \code{\link[R6]{R6Class}} to construct supervised tasks.
#' This is the abstract base class, do not use directly!
#'
#' @template fields-task
#' @template fields-supervisedtask
#' @return [\code{\link{SupervisedTask}}].
#' @family Tasks
#' @export
SupervisedTask = R6Class("SupervisedTask",
  inherit = Task,
  public = list(
    target = NA_character_,
    initialize = function(data, target, cols = NULL, id.col = NULL, id = deparse(substitute(data))) {
      super$initialize(data, cols = cols, id.col = id.col, id = id)
      self$target = assertChoice(target, self$backend$cols)
    }
  ),
  active = list(
    formula = function() {
      reformulate(setdiff(self$backend$cols, self$target), response = self$target)
    },
    features = function() setdiff(self$backend$cols, self$target)
  )
)

getTask = function(x) {
  Tasks$get(x)
}

getTasks = function(x) {
  if (!is.list(x))
    x = list(x)
  res = lapply(x, getTask)
  setNames(res, ids(res))
}

#' @export
listTasks = function() {
  tab = rbindlist(lapply(Tasks$ids, function(id) {
    task = getTask(id)
    list(
      id = task$id,
      type = task$type,
      nrow = task$nrow,
      ncol = task$ncol
    )
  }))
  setkeyv(tab, "id")[]
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
