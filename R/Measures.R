#' @title Registered Resampling Methods
#' @docType class
#' @format \code{\link{R6Class}} object
#'
#' @description
#' \code{Measures} is a \code{\link{Dictionary}} used to manage performance measures.
#'
#' @include Dictionary.R
#' @export
Measures = R6Class("Measures", inherit = Dictionary)$new("Measure")

#' @export
as.data.table.Measures = function(x, keep.rownames = FALSE, ...) {
  tab = rbindlist(eapply(x$env, function(x) {
    list(
      id = x$id,
      task.types = list(x$task.types)
    )
  }, USE.NAMES = FALSE))
  setkeyv(tab, "id")[]
}
