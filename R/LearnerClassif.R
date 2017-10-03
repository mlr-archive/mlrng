#' @include Learner.R

LearnerClassif = R6Class("LearnerClassif",
  inherit = Learner,
  public = list(
    initialize = function(type, name, par.set, par.vals = list(), packages = character(0L), properties = character(0L), train, predict) {
      super$initialize(type, name, par.set, par.vals = list(), packages = character(0L), properties = character(0L), train, predict,
        allowed.predict.types = c("response", "prob"), predict.type = "response")
    }
  )
)

