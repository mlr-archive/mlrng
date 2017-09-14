#' @include Register.R

#' @title Registered Tasks
#' @docType class
#' @format \code{\link{R6Class}} object
#'
#' @description
#' A \code{\link[R6]{R6Class}} to manage tasks
#'
#' @field ids Returns the ids of registered tasks.
#' @field storage Environment where all \code{\link{Task}} objects are stored.
#' @section Methods:
#' \describe{
#'  \item{\code{exists(ids)}}{Returns \code{TRUE} if a \code{\link{Task}} with id \code{ids} is registered.}
#'  \item{\code{get(id)}}{Returns \code{\link{Task} with corresponding \code{id}}.}
#' }
#' @return [\code{\link{Register}}].
#' @export
Tasks = Register$new("Task")

#' @title Base Class for Tasks
#' @format \code{\link{R6Class}} object
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

#' @export
#' @rdname Tasks
getTask = function(x) {
  Tasks$get(x)
}

#' @export
#' @rdname Tasks
getTasks = function(x) {
  if (!is.list(x))
    x = as.list(x)
  res = lapply(x, getTask)
  setNames(res, ids(res))
}

#' @export
#' @rdname Tasks
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
