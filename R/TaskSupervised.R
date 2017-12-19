#' @include Task.R
TaskSupervised = R6Class("TaskSupervised",
  # Base Class for Supervised Tasks
  inherit = Task,
  public = list(
    initialize = function(id, data, target) {
      super$initialize(id = id, data = data)
      assertChoice(target, self$features) # FIXME: use data.table
      self$cols[id == target, "role" := "target"]
    },

    print = function(...) {
        gcat("Supervised Task
              Target: {self$target}")
        super$print()
    },

    truth = function(rows = NULL) {
      if (is.null(rows)) {
        role = NULL
        rows = self$rows[role == "training", "id"][[1L]]
      }
      self$backend$get(rows, cols = self$target)
    }
  ),

  active = list(
    target = function() {
      role = NULL
      self$cols[role == "target", "id"][[1L]]
    },

    # [formula]. target ~ x1 + ... + xp
    formula = function() {
      reformulate(self$features, response = self$target)
    }
  )
)
