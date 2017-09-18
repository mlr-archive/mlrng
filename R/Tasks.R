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
#' @return [\code{\link{Dictionary}}].
#' @include Dictionary.R
#' @export
Tasks = Dictionary$new("Task")

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
