Register = R6Class("Register",
  cloneable = FALSE,
  public = list(
    storage = NULL,
    contains = NA_character_,
    source = NA_character_,

    initialize = function(contains, source = "mlrng") {
      self$storage = new.env(parent = emptyenv())
      self$contains = contains
      self$source = source
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

      obj = get0(as.character(id), envir = self$storage, inherits = FALSE)
      if (is.null(obj)) {
        if (must.work)
          gstop("{self$contains} with id '{id}' not found")
        return(NULL)
      }

      if (inherits(obj, "LazyElement")) obj$get() else obj$clone(deep = TRUE)
    },

    exists = function(ids) {
      assertCharacter(ids, any.missing = FALSE)
      ids %chin% ls(self$storage, all.names = TRUE, sorted = FALSE)
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
