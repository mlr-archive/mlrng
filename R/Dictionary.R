Dictionary = R6Class("Dictionary",
  cloneable = TRUE,

  public = list(
    env = NULL, # env where objects are stored
    eltype = NULL, # container<type>, might be NULL, then all types are OK

    # construct, set container type (string)
    initialize = function(eltype) {
      assertString(eltype, null.ok = TRUE)
      self$env = new.env(parent = emptyenv())
      self$eltype = eltype
    },

    # register an new element, either take id (string) from object or set it manually
    # for the former we assume that obj$id works
    # FIXME: what happens if LazyElemet returns something wrong and not of eltype?
    # FIXME: we need to deep copy obj if the user adds stuff to it.
    add = function(obj, id = NULL, overwrite = FALSE) {
      if (!is.null(self$eltype) && !inherits(obj, "LazyElement")) # we cannot check type
        assertClass(obj, self$eltype)
      if (is.null(id)) id = obj$id else assertString(id)
      if (!overwrite && id %chin% self$ids)
        gstop("Id '{id}' already present in dictionary!")
      assign(x = id, value = obj, envir = self$env)
    },

    # get object from dict by id
    # [string] x [bool] --> eltype
    get = function(id, check = TRUE, deep = FALSE) {
      assertString(id)
      if (check) private$checkIdsContained(id)
      obj = get0(id, envir = self$env, inherits = FALSE)
      if (inherits(obj, "LazyElement")) obj$get() else obj$clone(deep = deep)
    },

    # are ids present in dic?
    # [charvec] --> [logical].
    contains = function(ids) {
      assertCharacter(ids, any.missing = FALSE)
      ids %chin% self$ids
    },

    # [charvec] --> X. Removes elements from the dict
    remove = function(ids, check = TRUE) {
      assertCharacter(ids, any.missing = FALSE)
      if (check) private$checkIdsContained(ids)
      rm(list = ids, envir = self$env)
      invisible(self)
    },

    # [charvec] --> X. Restricts to some elements from the dict
    restrict = function(ids, check = TRUE) {
      assertCharacter(ids, any.missing = FALSE)
      if (check) private$checkIdsContained(ids)
      rest = setdiff(self$ids, ids)
      self$remove(rest)
    }
  ),

  active = list(
    ids = function() ls(self$env, all.names = TRUE, sorted = TRUE),
    length = function() length(self$env)
  ),

  private = list(
    # check that all ids are in dict, otherwise report error
    checkIdsContained = function(ids) {
      allids = self$ids
      j = wf(!(ids %chin% allids))
      if (length(j) > 0L)
        gstop("{self$eltype} with id '{ids[j]}' not found!")
    },

    # deep-clones each element in env
    # FIXME: we should agree that we store R6, nothing else?
    deep_clone = function(name, value) {
      if (name == "env")
        list2env(eapply(value, function(x) if (inherits(x, "R6")) x$clone(deep = TRUE) else x), parent = emptyenv())
      else
        value
    }
  )
)


# FIXME: summary does not work for empty reg, --> unit test
#' @export
summary.Dictionary= function(object, ...) {
  ids = object$ids
  gmessage("
    Dictionary of {length(ids)} {object$eltype}
    Ids: {convertToShortString(ids)}
  ")
}

#' @export
as.list.Dictionary = function(x, ...) {
  as.list(x$env)
}

#' @export
as.data.table.Dictionary = function(x, keep.rownames = FALSE, ...) {
  tab = rbindlist(eapply(x$env, function(e) {
    x = if (inherits(e, "LazyElement")) e$get() else e$clone()
    list(
      id = e$id,
      obj = list(x)
    )
  }, USE.NAMES = FALSE))
  setkeyv(tab, "id")[]
}



# class to define lazy objects, which can be expanded / allocated later
# FIXME: dont we want to specify the type here that get returns?
# then check class an get()
LazyElement = R6Class("LazyElement",
  cloneable = FALSE,
  public = list(
    id = NA_character_, # key (string)
    get = NULL,         # function() to construct
    cl = NULL,
    initialize = function(id, get, cl = NULL) {
      self$id = assertString(id, min.chars = 1L)
      self$get = assertFunction(get, args = character(0L))
    }
  )
)
