#' @title Basic Tasks
#' @format \code{\link{R6Class}} object
#'
#' @description
#' This is the abstract base class for task objects.
#' Use \code{\link{TaskClassif}} or \code{\link{TaskRegr}} to construct tasks instead of this class.
#'
#' @template fields-task
#' @template fields-supervisedtask
#'
#' @return [\code{\link{Task}}].
#' @export
#' @family Tasks
#' @examples
#' task = Task$new("iris", data = iris)
#' task$formula
Task = R6Class("Task",
  # Base Class for Tasks
  public = list(
    ### SLOTS ##################################################################
    task.type = NA_character_,
    id = NULL,
    backend = NULL,
    rows = NULL,
    cols = NULL,

    ### METHODS ################################################################
    initialize = function(id, data) {
      self$id = assertString(id, min.chars = 1L)
      if (is.data.frame(data)) {
        self$backend = BackendDBI$new(data = data, tbl.name = id)
      } else {
        self$backend = assertR6(data, "Backend")
      }

      self$rows = data.table(id = self$backend$rownames, role = "training", key = "id")

      cols = data.table(id = self$backend$colnames, key = "id")
      cols$role = ifelse(cols$id == self$backend$rowid.col, "primary.id", "feature")
      types = vcapply(self$backend$head(1L), class)
      cols$type = types[match(cols$id, names(types), 0L)]
      self$cols = cols
    },

    get = function(rows = NULL, cols = NULL) {
      if (is.null(rows)) {
        if (self$rows[role == "training", .N] == nrow(self$rows)) {
          # not necessarily required ... but lessens the burden on the data base
          selected.rows = NULL
        } else {
          selected.rows = self$rows[role == "training", "id"][[1L]]
        }
      } else {
        selected.rows = self$rows[role == "training"][.(rows), "id"][[1L]]
        if (length(selected.rows) != length(rows))
          stop("Invalid row ids provided")
      }
      if (is.null(cols)) {
        selected.cols = self$cols[role %in% c("feature", "target"), "id"][[1L]]
      } else {
        selected.cols = self$cols[role %in% c("feature", "target")][.(cols), "id"][[1L]]
        if (length(selected.cols) != length(cols))
          stop("Invalid col ids provided")
      }
      self$backend$get(rows = selected.rows, cols = selected.cols)
    },

    head = function(n = 6L) {
      assertCount(n)
      self$backend$head(n)[, self$features, with = FALSE]
    },

    print = function(...) {
      features = self$features
      types = glue_data(self$cols[features, .N, keyby = "type"], "{N} {type}")
      gcat("Task name: {self$id}
            {self$nrow} rows and {length(features)} features.
            Features: {stri_peek(features)}
            Types: {stri_flatten(types, \", \")}")
      if (getOption("mlrng.debug", FALSE))
          cat("\n", format(self), "\n")
  }),

  ### ACTIVE ##################################################################
  active = list(
    data = function(newdata) {
      if (missing(newdata)) {
        role = NULL
        cols = self$cols[role %in% c("feature", "target"), "id"][[1L]]
        rows = self$rows[role == "training", "id"][[1L]]
        return(self$get(rows = rows, cols = cols))
      }

      if (inherits(self$backend, "BackendLocal")) {
        self$backend$data = newdata
      } else {
        if (getOption("mlrng.debug", FALSE))
          gmessage("Creating an in-memory copy of task '{self$id}'")
        self$backend = BackendLocal$new(data = newdata, rowid.col = self$backend$rowid.col)
      }

      # subset rows and cols to those present in newdata
      self$rows = setkeyv(self$rows[.(self$backend$rownames), nomatch = 0L, on = "id"], "id")
      self$cols = setkeyv(self$cols[.(self$backend$colnames), nomatch = 0L, on = "id"], "id")
    },

    # [charvec]. feature names without target names
    features = function() {
      role = NULL
      features = self$cols[role == "feature", "id"][[1L]]
    },

    target = function() {
      character(0L)
    },

    formula = function() {
      reformulate(self$features)
    },

    nrow = function() {
      role = NULL
      self$rows[role == "training", .N]
    },

    ncol = function() {
      role = NULL
      self$cols[role %in% c("feature", "target"), .N]
    },

    col.types = function() {
      cols = self$cols[role %in% c("feature", "target"), c("id", "type")]
      setNames(cols$type, cols$id)
    }
  ),
  private = list(
    deep_clone = function(name, value) {
      if (name %chin% c("rows", "cols"))
        return(copy(value))
      return(value)
    }
  )
)
