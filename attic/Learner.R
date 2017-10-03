#' @title Class for Learners
#' @format \code{\link{R6Class}} object
#'
#' @description
#' A \code{\link[R6]{R6Class}} to construct learners.
#'
#' @return [\code{\link{Learner}}].
#' @family Learner
#' @export
Learner = R6Class("Learner",
  public = list(
    name = NA_character_,
    id = NA_character_,
    type = NA_character_,
    packages = NA_character_,
    par.set = list(),
    properties = character(0L),
    train = NULL,
    predict = NULL,

    initialize = function(type, name, par.set, par.vals = list(), predict.type = "response", packages = character(0L), properties = character(0L), train, predict) {
      self$type = assertString(type)
      self$name = assertString(name)
      self$id = stri_paste(type, ".", name)
      self$par.set = assertClass(par.set, "ParamSet")
      private$pt = assertChoice(predict.type, choices = switch(self$type, classif = c("response",
        "prob"), multilabel = c("response", "prob"), regr = c("response",
          "se"), surv = c("response", "prob"), costsens = "response",
        cluster = c("response", "prob")))
      private$pv = assertList(par.vals, names = "unique")
      self$packages = assertCharacter(packages, any.missing = FALSE, unique = TRUE)
      self$properties = assertCharacter(properties, any.missing = FALSE, unique = TRUE)
      self$train = assertFunction(train, args = c("task", "subset"), ordered = TRUE)
      self$predict = assertFunction(predict, args = c("model", "task", "subset"), ordered = TRUE)
      environment(self$train) = environment(self$predict) = environment(self$initialize)
    }
  ),
  active = list(
    par.vals = function(rhs) {
      if (missing(rhs))
        return(private$pv)
      assertList(rhs, names = "unique")
      assertSubset(names(rhs), getParamIds(self$par.set))
      private$pv[names(rhs)] = rhs
    },
    predict.type = function(rhs) {
      if (missing(rhs))
        return(private$pt)
      assertChoice(rhs, choices = switch(self$type, classif = c("response",
        "prob"), multilabel = c("response", "prob"), regr = c("response",
          "se"), surv = c("response", "prob"), costsens = "response",
        cluster = c("response", "prob")))
      private$pt = rhs
    }
  ),
  private = list(
    pv = NULL,
    pt = NULL
  )
)
