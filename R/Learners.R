#' @include Dictionary.R

#' @title Registered Learners
#' @docType class
#' @format \code{\link{R6Class}} object
#'
#' @description
#' A \code{\link[R6]{R6Class}} to manage learners.
#'
#' @field ids Returns the ids of registered learners.
#' @field storage Environment where all \code{\link{Learner}} objects are stored.
#' @section Methods:
#' \describe{
#'  \item{\code{exists(ids)}}{Returns \code{TRUE} if a learner with id \code{ids} is registered.}
#'  \item{\code{get(id)}}{Returns \code{\link{Learner} with corresponding \code{id}}.}
#' }
#' @return [\code{\link{Dictionary}}].
#' @export
#' @examples
#' Learners$ids
#' Learners$exists("classif.dummy")
#' Learners$get("classif.dummy")
Learners = Dictionary$new("Learner")

#' @export
#' @rdname Learners
listLearners = function() {
  tab = rbindlist(eapply(Learners$env, function(lrn) {
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
