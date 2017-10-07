#' @include Learner.R
#' @export
LearnerClassif = R6Class("LearnerClassif",
  inherit = Learner,
  public = list(
    initialize = function(name, par.set, par.vals = list(), packages = character(0L), properties = character(0L), train, predict, model.extractors = list()) {
      super$initialize("classif", name, par.set, par.vals, packages, properties, train, predict,
        model.extractors, allowed.predict.types = c("response", "prob"), predict.type = "response")
    }
  )
)

