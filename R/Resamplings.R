#' @title Registered Resampling Methods
#' @docType class
#' @format \code{\link{R6Class}} object
#'
#' @description
#' \code{Resamplings} is a \code{\link{Dictionary}} used to manage resampling methods.
#'
#' @include Dictionary.R
#' @export
Resamplings = R6Class("Resamplings", inherit = Dictionary)$new("Resampling")

#' @export
as.data.table.Resamplings = function(x, keep.rownames = FALSE, ...) {
  tab = rbindlist(eapply(Resamplings$env, function(r) {
    list(
      id = r$id,
      description = r$description,
      pars = list(r$pars)
    )
  }, USE.NAMES = FALSE))
  setkeyv(tab, "id")[]
}
