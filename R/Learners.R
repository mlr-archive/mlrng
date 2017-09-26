#' @title Registered Learners
#' @docType class
#' @format \code{\link{R6Class}} object
#'
#' @description
#' \code{Learners} is a \code{\link{Dictionary}} used to manage learners.
#'
#' @include Dictionary.R
#' @export
#' @examples
#' Learners$ids
#' Learners$contains("classif.dummy")
#' Learners$get("classif.dummy")
Learners = R6Class("Learners", inherit = Dictionary)$new("Learner")

#' @export
as.data.table.Learners = function(x, keep.rownames = FALSE, ...) {
  tab = rbindlist(eapply(x$env, function(lrn) {
    list(
      id = lrn$id,
      name = lrn$name,
      type = lrn$type,
      properties = list(lrn$properties),
      packages = list(lrn$packages)
    )
  }, USE.NAMES = FALSE))
  setkeyv(tab, "id")[]
}
