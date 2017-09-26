#' @title Registered Tasks
#' @docType class
#' @format \code{\link{R6Class}} object
#'
#' @description
#' \code{Tasks} is a \code{\link{Dictionary}} used to manage tasks.
#'
#' @include Dictionary.R
#' @export
Tasks = R6Class("Tasks", inherit = Dictionary)$new("Task")

#' @export
as.data.table.Tasks = function(x, keep.rownames = FALSE, ...) {
  tab = rbindlist(lapply(x$ids, function(id) {
    task = x$get(id)
    list(
      id = task$id,
      type = task$type,
      nrow = task$backend$nrow,
      ncol = task$backend$ncol
    )
  }))
  setkeyv(tab, "id")[]
}
