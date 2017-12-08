#' @include Task.R
TaskSupervised = R6Class("TaskSupervised",
  # Base Class for Supervised Tasks
  inherit = Task,
  public = list(
    target = NA_character_,

    initialize = function(id, data, target) {
      super$initialize(id = id, data = data)
      self$target = assertChoice(target, self$backend$colnames)
    },

    print = function(...) {
        gcat("Supervised Task
              Target: {self$target}")
        super$print()
    },

    truth = function(rows = NULL) {
      self$backend$get(rows, cols = self$target)
    }

  ),

  active = list(
    # [formula]. target ~ x1 + ... + xp
    formula = function() {
      reformulate(self$feature.names, response = self$target)
    },

    # [charvec]. feature names without target names
    feature.names = function() {
      setdiff(self$backend$colnames, self$target)
    }
  )
)
