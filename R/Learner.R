#' @title Create a Learner
#' @include Register.R
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
#' @return [\code{\link{Register}}].
#' @export
#' @examples
#' \dontrun{
#' Learners$ids
#' Learners$exists("classif.dummy")
#' Learners$get("classif.dummy")
#' }
Learners = Register$new("Learner")

Learner = R6Class("Learner",
  public = list(
    name = NA_character_,
    id = NA_character_,
    type = NA_character_,
    packages = NA_character_,
    par.set = list(),
    par.vals = list(),
    properties = character(0L),
    train = NULL,
    predict = NULL,
    hooks = list(),

    initialize = function(type, name, par.set, par.vals = list(), packages = character(0L), properties = character(0L), train, predict) {
      self$type = assertString(type)
      self$name = assertString(name)
      self$id = stri_paste(type, ".", name)
      self$par.set = assertClass(par.set, "ParamSet")
      self$par.vals = assertList(par.vals, names = "unique")
      self$packages = assertCharacter(packages, any.missing = FALSE, unique = TRUE)
      self$properties = assertCharacter(properties, any.missing = FALSE, unique = TRUE)
      self$train = assertFunction(train, args = c("task", "subset"), ordered = TRUE)
      self$predict = assertFunction(predict, args = c("model", "task", "subset"), ordered = TRUE)
    },

    setHyperPars = function(par.vals) {
      assertList(par.vals, names = "unique")
      assertSubset(names(par.vals), getParamIds(self$par.set))
      self$par.vals[names(par.vals)] = par.vals
    },

    addHook = function(id, hooks) {
      assertString(id)
      assertList(hooks, names = "unique")
      assertSubset(names(hooks), c("pre.train", "post.train", "pre.predict", "post.predict", "pars"))
      self$hooks = c(self$hooks, setNames(list(hooks), id))
    }
  )
)

getLearner = function(x, ...) {
  x = Learners$get(x)
  if (...length() == 0L)
    x$setHyperPars(list(...))
  x
}

getLearners = function(x, ...) {
  if (!is.list(x))
    x = list(x)
  res = lapply(x, Learners$get, ...)
  setNames(res, ids(res))
}

#' @export
listLearners = function() {
  tab = rbindlist(eapply(Learners$storage, function(lrn) {
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
