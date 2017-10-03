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
    allowed.predict.types = NULL,

    initialize = function(type, name, par.set, par.vals = list(), packages = character(0L), properties = character(0L), train, predict, allowed.predict.types, predict.type) {
      self$type = assertString(type)
      self$name = assertString(name)
      self$id = stri_paste(type, ".", name)
      self$par.set = assertClass(par.set, "ParamSet")
      private$pv = assertList(par.vals, names = "unique")
      self$packages = assertCharacter(packages, any.missing = FALSE, unique = TRUE)
      self$properties = assertCharacter(properties, any.missing = FALSE, unique = TRUE)
      self$train = assertFunction(train, args = c("task", "subset"), ordered = TRUE)
      self$predict = assertFunction(predict, args = c("model", "task", "subset"), ordered = TRUE)
      environment(self$train) = environment(self$predict) = environment(self$initialize)
      self$allowed.predict.types = assertCharacter(allowed.predict.types, any.missing = FALSE, min.len = 1L)
      self$predict.type = assertChoice(predict.type, allowed.predict.types)
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
      assertChoice(rhs, self$allowed.predict.types)
      private$pt = rhs
    }
  ),
  private = list(
    pv = NULL,
    pt = NULL
  )
)
