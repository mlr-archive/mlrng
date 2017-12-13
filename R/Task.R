rowintersect = function(x, y, key) {
  # y is data.table, x is atomic vector
  if (is.null(x))
    return(y[[1L]])
  if (is.null(y))
    return(x)
  y[list(x), nomatch = 0L, on = key][[1L]]
}

colintersect = function(x, y) {
  if (is.null(x))
    return(y)
  if (is.null(y))
    return(x)
  unique(y[chmatch(x, y, 0L)])
}


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
    roles = NULL,

    ### METHODS ################################################################
    initialize = function(id, data) {
      self$id = assertString(id, min.chars = 1L)
      if (is.data.frame(data)) {
        self$backend = BackendDBI$new(data = data, tbl.name = id)
      } else {
        self$backend = assertR6(data, "Backend")
      }
      self$roles = list(
        cols = data.table(id = self$backend$colnames, key = "id"),
        rows = data.table(id = self$backend$rownames, role = "training", key = "id")
      )
      self$roles$cols$role = ifelse(self$roles$cols$id == self$backend$rowid.col, "primary.id", "feature")
      types = vcapply(self$backend$head(1L), class)
      self$roles$cols$type = types[match(self$roles$cols$id, names(types), 0L)]
    },

    get = function(rows = NULL, cols = NULL) {
      self$backend$get(rows = rows, cols = cols)
    },

    rows = function(roles = NULL) {
      if (is.null(roles))
        return(self$roles$rows[["id"]])
      assertCharacter(roles, any.missing = FALSE)
      self$roles$rows[role %in% roles, "id", with = FALSE][[1L]]
    },

    cols = function(roles = NULL, types = NULL) {
      assertCharacter(roles, any.missing = FALSE, null.ok = TRUE)
      assertCharacter(types, any.missing = FALSE, null.ok = TRUE)

      switch((!is.null(roles)) + 2L * (!is.null(types)) + 1L,
        self$roles$cols,
        self$roles$cols[role %in% roles],
        self$roles$cols[type %in% types],
        self$roles$cols[role %in% roles & type %in% types],
      )[, "id", with = FALSE][[1L]]
    },

    head = function(n = 6L) {
      assertCount(n)
      self$backend$head(n)[, self$cols(roles = "feature"), with = FALSE]
    },

    subset = function(rows = NULL, cols = NULL) {
      self$backend$subset(rows, cols)
      invisible(self)
    },

    print = function(...) {
      feats = self$cols(roles = "feature")
      types = glue_data(self$roles$cols[feats, .N, keyby = "type"], "{N} {type}")
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
        return(self$backend$data)
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
      self$backend$colnames
    },

    formula = function() {
      reformulate(self$features)
    },

    nrow = function() {
      self$backend$nrow
    },

    ncol = function() {
      self$backend$ncol
    },

    col.types = function() {
      self$backend$types
    },

    missing.values = function() {
      self$backend$missing.values
    }
  )
)
