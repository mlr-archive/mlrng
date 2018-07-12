#' @title Class for Learners
#' @format [R6Class()] object
#'
#' @description
#' A [R6::R6Class()] to construct learners.
#'
#' @return [[Learner()]].
#' @family Learner
#' @export
Learner = R6Class("Learner",
  public = list(
    name = NA_character_,  # immutable
    id = NA_character_,  # user can change this, by default this is <task.type>.<name>
    task.type = NA_character_,  # immutable
    packages = NA_character_,
    par.set = list(),
    properties = character(0L),
    train = NULL,
    predict = NULL,
    model.extractors = list(),
    allowed.predict.types = NULL,

    initialize = function(task.type, name, par.set, par.vals, packages, properties, train, predict, model.extractors, allowed.predict.types, predict.type) {
      self$task.type = assertString(task.type)
      self$name = assertString(name)
      self$id = stri_paste(task.type, ".", name)
      self$par.set = assertClass(par.set, "ParamSet")
      private$pv = assertList(par.vals, names = "unique")
      self$packages = assertCharacter(packages, any.missing = FALSE, unique = TRUE)
      self$properties = assertCharacter(properties, any.missing = FALSE, unique = TRUE)
      self$train = assertFunction(train, args = c("task", "subset"), ordered = TRUE)
      self$predict = assertFunction(predict, args = c("model", "newdata"), ordered = TRUE)
      self$model.extractors = lapply(model.extractors,
        function(m) assertFunction(m, args = c("model", "task", "subset"), ordered = TRUE, null.ok = TRUE))
      self$allowed.predict.types = assertCharacter(allowed.predict.types, any.missing = FALSE, min.len = 1L)
      self$predict.type = assertChoice(predict.type, allowed.predict.types)

      # set environments for functions
      if (length(self$model.extractors) > 0)
        for (i in seq_along(self$model.extractors))
          environment(self$model.extractors[[i]]) = environment(self$initialize)
      environment(self$train) = environment(self$predict) = environment(self$initialize)
    },
    print = function(...) {
      gcat("Learner {self$id} from package {self$packages}.
            Predict type: {self$predict.type}.
            Properties: {stri_peek(self$properties)}
            Extractors: {stri_peek(names(self$model.extractors))}")
      if (getOption("mlrng.debug", FALSE))
        cat("\n", format(self), "\n")
    }
  ),
  active = list(
    par.vals = function(rhs) {
      if (missing(rhs))
        return(private$pv)
      assertList(rhs, names = "unique")
      assertSubset(names(rhs), self$par.set$ids)
      private$pv[names(rhs)] = rhs
    },
    predict.type = function(rhs) {
      if (missing(rhs))
        return(private$pt)
      assertChoice(rhs, self$allowed.predict.types)
      private$pt = rhs
    }
  ),
  private = list(
    pv = NULL,
    pt = NULL
  )
)
