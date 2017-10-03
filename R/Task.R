#' @title Base Class for Tasks
#' @format \code{\link{R6Class}} object
#'
#' @description
#' A \code{\link[R6]{R6Class}} to construct tasks.
#' This is the abstract base class, do not use directly!
#'
#' @template fields-task
#' @return [\code{\link{Task}}].
#' @family Tasks
Task = R6Class("Task",
  public = list(
    ### SLOTS ##################################################################
    id = NULL,
    view = NULL,

    ### METHODS ################################################################
    initialize = function(id, data) {
      self$id = assertString(id, min.chars = 1L)
      if (is.data.frame(data)) {
        self$view = asView(name = id, data = data)
      } else {
        self$view = assertR6(data, "View")
      }
    },

    data = function(rows = NULL, cols = NULL) {
      setDT(self$view$data(rows, cols))[]
    },

    head = function(n = 6L) {
      setDT(dplyr::collect(head(self$view$tbl, n = n)))[]
    },

    deep_clone = function(name, value) {
      if (name == "view") value$clone(deep = TRUE) else value
    }
  ),

  ### ACTIVE ##################################################################
  active = list(
    nrow = function() {
      self$view$nrow
    },

    ncol = function() {
      self$view$ncol
    },

    col.types = function() {
      self$view$types
    },

    na.cols = function() {
      res = dplyr::summarize(self$view$tbl, dplyr::funs(sum(is.na(.))))
      unlist(dplyr::collect(res))
    }
  )
)

getTaskSupervisedData = function(task, type, subset, learner.props = NULL) {
  task = mlr.tasks$get("bh")
  type = "train"
  assertR6(task, "TaskSupervised")
  assertChoice(type, c("train", "test", "extra"))
  assertAtomicVector(subset, any.missing = FALSE)
  x = task$data(rows = subset, cols = task$active.cols)

  if (!is.null(learner.probs)) {
    mutate_if = function(x, predicate, conv, ...) {
      nn = names(which(vlapply(x, is.character)))
      if (length(nn)) {
        conv = match.fun(conv)
        x[, nn := lapply(.SD, conv, ...), .SDcols = nn]
      }
    }

    if ("logical" %chnin% learner.probs)
      x = mutate_if(x, is.logical, as.integer)
    if ("integer" %chnin% learner.probs)
      x = mutate_if(x, is.integer, as.double)
    if ("character" %chin% learner.probs && "factor" %chnin% learner.probs)
      x = mutate_if(x, is.factor, as.character)
    if ("character" %chnin% learner.probs && "factor" %chin% learner.probs)
      x = mutate_if(x, is.character, as.factor)
  }

  return(x)
}
