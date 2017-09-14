Register = R6Class("Register",
  cloneable = FALSE,
  public = list(
    storage = NULL,
    contains = NA_character_,

    initialize = function(contains) {
      self$storage = new.env(parent = emptyenv())
      self$contains = contains
    },

    register = function(obj, id = NULL) {
      if (!inherits(obj, "LazyElement"))
        assertClass(obj, self$contains)
      if (is.null(id))
        id = obj$id
      assign(x = id, value = obj, envir = self$storage)
    },

    get = function(id, must.work = TRUE) {
      if (inherits(id, self$contains))
        return(id)

      assertString(id)
      obj = get0(id, envir = self$storage, inherits = FALSE)
      if (is.null(obj)) {
        if (must.work)
          gstop("{self$contains} with id '{id}' not found")
        return(NULL)
      }

      if (inherits(obj, "LazyElement")) obj$get() else obj$clone(deep = TRUE)
    },

    mget = function(ids) {
      if (is.character(ids)) {
        ids = lapply(ids, self$get)
      } else if (inherits(ids, self$contains)) {
        ids = list(ids)
      } else {
        ids = lapply(ids, self$get)
      }
      setNames(ids, ids(ids))
    },

    exists = function(ids) {
      assertCharacter(ids, any.missing = FALSE)
      ids %chin% ls(self$storage, all.names = TRUE, sorted = FALSE)
    },

    remove = function(ids) {
      assertCharacter(ids, any.missing = FALSE)
      rm(list = ids, envir = self$storage)
    },

    print = function(...) {
      gcat("
        Register of {length(self$ids)} objects of class `{self$contains}`:

        {format(self)}
      ")
    }
  ),

  active = list(
    ids = function() {
      ls(self$storage, all.names = TRUE, sorted = TRUE)
    }
  )
)

#' @export
as.data.table.Register = function(x, keep.rownames = FALSE, ...) {
  tab = rbindlist(eapply(x$storage, function(e) {
    x = if (inherits(e, "LazyElement")) e$get() else e$clone()
    list(
      id = e$id,
      obj = list(x)
    )
  }, USE.NAMES = FALSE))
  setkeyv(tab, "id")[]
}

LazyElement = R6Class("LazyElement",
  cloneable = FALSE,
  public = list(
    id = NA_character_,
    get = NULL,
    initialize = function(id, get) {
      self$id = assertString(id, min.chars = 1L)
      self$get = assertFunction(get, args = character(0L))
    }
  )
)
