#' @include Task.R
TaskSupervised = R6Class("TaskSupervised",
  # Base Class for Supervised Tasks
  inherit = Task,
  public = list(
    initialize = function(id, data, target) {
      super$initialize(id = id, data = data)
      assertChoice(target, self$cols(roles = "feature")) # FIXME: use data.table
      self$col.roles[id == target, "role" := "target"]
    },

    print = function(...) {
        gcat("Supervised Task
              Target: {self$target}")
        super$print()
    },

    truth = function(rows = self$rows("training")) {
      # FIXME: could be optimized to not select all rows
      self$backend$get(rows, cols = self$target)
    }
  ),

  active = list(
    target = function() {
      self$cols(roles = "target")
    },

    # [charvec]. feature names without target names
    features = function() {
      self$cols(roles = "feature")
    },

    # [formula]. target ~ x1 + ... + xp
    formula = function() {
      reformulate(self$features, response = self$target)
    }
  )
)
