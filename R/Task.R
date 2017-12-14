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
    row.roles = NULL,
    col.roles = NULL,

    ### METHODS ################################################################
    initialize = function(id, data) {
      self$id = assertString(id, min.chars = 1L)
      if (is.data.frame(data)) {
        self$backend = BackendDBI$new(data = data, tbl.name = id)
      } else {
        self$backend = assertR6(data, "Backend")
      }

      self$row.roles = data.table(id = self$backend$rownames, role = "training", key = "id")

      col.roles = data.table(id = self$backend$colnames, key = "id")
      col.roles$role = ifelse(col.roles$id == self$backend$rowid.col, "primary.id", "feature")
      types = vcapply(self$backend$head(1L), class)
      col.roles$type = types[match(col.roles$id, names(types), 0L)]
      self$col.roles = col.roles
    },

    get = function(rows = NULL, cols = NULL) {
      if (is.null(rows)) {
        if (self$row.roles[role == "training", .N] == nrow(self$row.roles)) {
          # not necessarily required ... but lessens the burden on the data base
          selected.rows = NULL
        } else {
          selected.rows = self$row.roles[role == "training", "id"][[1L]]
        }
      } else {
        selected.rows = self$row.roles[role == "training"][.(rows), "id"][[1L]]
        if (length(selected.rows) != length(rows))
          stop("Invalid row ids provided")
      }
      if (is.null(cols)) {
        selected.cols = self$col.roles[role %in% c("feature", "target"), "id"][[1L]]
      } else {
        selected.cols = self$col.roles[role %in% c("feature", "target")][.(cols), "id"][[1L]]
        if (length(selected.cols) != length(cols))
          stop("Invalid col ids provided")
      }
      self$backend$get(rows = selected.rows, cols = selected.cols)
    },

    rows = function(roles = "training") {
      assertSubset(roles, mlrng$supported.row.roles, fmatch = TRUE)
      self$row.roles[role %in% roles, "id", with = FALSE][[1L]]
    },

    cols = function(roles = NULL, types = NULL) {
      assertCharacter(roles, any.missing = FALSE, null.ok = TRUE)
      assertCharacter(types, any.missing = FALSE, null.ok = TRUE)

      role = type = NULL
      switch((!is.null(roles)) + 2L * (!is.null(types)) + 1L,
        self$col.roles,
        self$col.roles[role %in% roles],
        self$col.roles[type %in% types],
        self$col.roles[role %in% roles & type %in% types],
      )[, "id", with = FALSE][[1L]]
    },

    head = function(n = 6L) {
      assertCount(n)
      self$backend$head(n)[, self$cols(roles = "feature"), with = FALSE]
    },

    print = function(...) {
      feats = self$cols(roles = "feature")
      types = glue_data(self$col.roles[feats, .N, keyby = "type"], "{N} {type}")
      gcat("Task name: {self$id}
            {self$nrow} rows and {length(feats)} features.
            Features: {stri_peek(feats)}
            Types: {stri_flatten(types, \", \")}")
      if (getOption("mlrng.debug", FALSE))
          cat("\n", format(self), "\n")
  }),

  ### ACTIVE ##################################################################
  active = list(
    data = function(newdata) {
      if (missing(newdata)) {
        return(self$get(rows = self$rows(role = "training"), cols = self$cols(role = c("feature", "target"))))
      }

      if (inherits(self$backend, "BackendLocal")) {
        self$backend$data = newdata
      } else {
        if (getOption("mlrng.debug"))
          gmessage("Creating an in-memory copy of task '{self$id}'")
        self$backend = BackendLocal$new(data = newdata, rowid.col = self$backend$rowid.col)
      }
    },

    # [charvec]. feature names without target names
    features = function() {
      self$cols(roles = "feature")
    },

    formula = function() {
      reformulate(self$cols(roles = "feature"))
    },

    nrow = function() {
      role = NULL
      self$row.roles[role == "training", .N]
    },

    ncol = function() {
      role = NULL
      self$col.roles[role %in% c("feature", "target"), .N]
    },

    col.types = function() {
      cols = self$col.roles[role %in% c("feature", "target"), c("id", "type")]
      setNames(cols$type, cols$id)
    }
  ),
  private = list(
    deep_clone = function(name, value) {
      if (name %chin% c("row.roles", "col.roles")) copy(value) else value
    }
  )
)
