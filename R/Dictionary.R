#' @title Base Class for Dictionaries
#' @format \code{\link{R6Class}} object
#'
#' @description
#' A \code{\link[R6]{R6Class}} for a simple dictionary (hash map).
#' This is used to store objects like \code{\link{mlr.tasks}}, \code{\link{mlr.learners}},
#' \code{\link{mlr.resamplings}} or \code{\link{mlr.measures}}.
#'
#' @field ids Returns the ids of registered learners.
#' @field env Environment where all \code{\link{Learner}} objects are stored.
#' @section Methods:
#' \describe{
#'  \item{\code{add(obj, id, overwrite)}}{Add an object to the dictionary.}
#'  \item{\code{contains(ids)}}{Returns a logical vector signaling if objects with the respective id are stored inside the Dictionary.}
#'  \item{...}{...}
#' }
#'
#' @return [\code{Dictionary}].
Dictionary = R6Class("Dictionary",
  cloneable = TRUE,

  public = list(
    env = NULL, # env where objects are stored
    eltype = NULL, # container<type>, might be NULL, then all types are OK

    # construct, set container type (string)
    initialize = function(eltype) {
      self$eltype = assertString(eltype, null.ok = TRUE)
      self$env = new.env(parent = emptyenv())
    },

    # register an new element, either take id (string) from object or set it manually
    # for the former we assume that obj$id works
    # FIXME: what happens if LazyElemet returns something wrong and not of eltype?
    add = function(obj, id = NULL, overwrite = TRUE) {
      if (!is.null(self$eltype) && !inherits(obj, "LazyElement")) # we cannot check type
        assertClass(obj, self$eltype)
      if (is.null(id)) id = obj$id else assertString(id)
      if (!overwrite && self$contains(id))
        gstop("Id '{id}' already present in dictionary!", .call = FALSE)
      if (testR6(obj, cloneable = TRUE))
        obj = obj$clone(deep = TRUE)
      assign(x = id, value = obj, envir = self$env)
    },

    # get object from dict by id
    # [string] x [bool] --> eltype
    get = function(id, clone = TRUE) {
      assertString(id)
      assertContains(self, id)
      obj = get0(id, envir = self$env, inherits = FALSE)
      if (inherits(obj, "LazyElement"))
        return(obj$get())
      if (clone)
        return(obj$clone(deep = TRUE))
      return(obj)
    },

    # are ids present in dict?
    # [charvec] --> [logical].
    contains = function(ids) {
      vlapply(ids, exists, envir = self$env, inherits = FALSE)
    },

    # [charvec] --> X. Removes elements from the dict
    remove = function(ids) {
      assertCharacter(ids, any.missing = FALSE)
      assertContains(self, ids)
      rm(list = ids, envir = self$env)
      invisible(self)
    },

    # [charvec] --> X. Restricts to some elements from the dict
    slice = function(ids) {
      assertCharacter(ids, any.missing = FALSE)
      assertContains(self, ids)
      rm(list = setdiff(ls(self$env, all.names = TRUE, sorted = FALSE), ids), envir = self$env)
      invisible(self)
    },

    summary = function(ids) {
      assertContains(self, ids)
      data.table(id = ids, key = "id")
    },

    print = function(...) {
      ids = self$ids
      gcat("
        Dictionary of {length(ids)} {self$eltype}
        Ids: {stri_peek(ids)}
      ")
      if (getOption("mlrng.debug", FALSE))
        cat("\n", format(self), "\n")
    }
  ),

  active = list(
    ids = function() ls(self$env, all.names = TRUE, sorted = TRUE),
    length = function() length(self$env)
  ),

  private = list(
    # deep-clones each element in env
    deep_clone = function(name, value) {
      if (name == "env")
        list2env(eapply(value, function(x) if (inherits(x, "R6")) x$clone(deep = TRUE) else x), parent = emptyenv())
      else
        value
    }
  )
)

extractSummary = function(self, ids, fun) {
  if (is.null(ids)) {
    ids = self$ids
  } else {
    assertContains(self, ids)
  }
  fun = match.fun(fun)
  s = lapply(ids, function(id) c(list(id = id), fun(self$get(id, clone = FALSE))))
  setkeyv(rbindlist(s), "id")[]
}

#' @export
as.list.Dictionary = function(x, ...) {
  # FIXME: need to copy?
  as.list(x$env)
}

#' @export
as.data.table.Dictionary = function(x, keep.rownames = FALSE, ...) {
  ids = x$ids
  data.table(
    id = ids,
    object = lapply(ids, x$get, clone = TRUE)
  )
}

# class to define lazy objects, which can be expanded / allocated later
LazyElement = R6Class("LazyElement",
  cloneable = FALSE,
  public = list(
    id = NA_character_, # key (string)
    get = NULL,         # function() to construct
    initialize = function(id, get) {
      self$id = assertString(id, min.chars = 1L)
      self$get = assertFunction(get, args = character(0L))
    }
  )
)

assertContains = function(dict, keys) {
  j = wf(!dict$contains(keys))
  if (length(j) > 0L)
    gstop("{dict$eltype} with id '{keys[j]}' not found!", .call = FALSE)
}
