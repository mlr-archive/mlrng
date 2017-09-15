#FIXME: PROPERLY document what cloning means here, deep and non-deep

Dictionary = R6Class("Dictionary",
  cloneable = TRUE,

  public = list(
    env = NULL, # env where objects are stored
    eltype = NULL, # container<type>, might be NULL, then all types are OK
    clone.on.get = NULL,

    # construct, set container type (string)
    initialize = function(eltype, clone.on.get = FALSE) {
      assertString(eltype, null.ok = TRUE)
      assertFlag(clone.on.get)
      self$env = new.env(parent = emptyenv())
      self$eltype = eltype
      self$clone.on.get = clone.on.get
    },

    # register an new element, either take id (string) from object or set it manually
    # for the former we assume that obj$id works
    # ids must be unique
    # FIXME:  what happens if LazyElemet returns sometinh wrong and not of eltype?
    add = function(obj, id = NULL) {
      if (!is.null(self$eltype) && !inherits(obj, "LazyElement")) # we cannot check type
        assertClass(obj, self$eltype)
      if (is.null(id)) id = obj$id else assertString(id)
      if (id %in% self$ids)
        gstop("Id '{id}' already present in dictionary!")
      assign(x = id, value = obj, envir = self$env)
    },

    # get object from dict by id
    # [string] x [bool] --> eltype
    get = function(id, check = TRUE, deepclone = self$deepclone.on.get) {
      assertString(id)
      if (check) private$checkIdsContained(id)
      obj = get0(id, envir = self$env, inherits = FALSE)
      if (inherits(obj, "LazyElement")) obj = obj$get()
      if (self$clone.on.get && inherits(obj, "R6"))
        obj = obj$clone(deep = deepclone)
      return(obj)
    },

    # subset dict with given ids
    # FIXME: is the deepclone arg good here?
    getSubset = function(ids, check = TRUE, deepclone = self$deepclone.on.get) {
      assertCharacter(ids, any.missing = FALSE)
      if (check) private$checkIdsContained(ids)
      d = self$clone(deep = FALSE)
      d$restrict(ids, check = FALSE)
      if (deepclone)
        d = d$clone(deep = TRUE)
      return(d)
    },

    # are ids present in dic?
    # [charvec] --> [logical].
    contains = function(ids) {
      assertCharacter(ids, any.missing = FALSE)
      ids %chin% self$ids
    },

    # FIXME:
    # containsObject = function(x)
    # wie vergleicht man R objecte am besten auf equals?
    # wie geht das in R6?


    # [charvec] --> X. removes elements from the dict
    remove = function(ids, check = TRUE) {
      assertCharacter(ids, any.missing = FALSE)
      if (check) private$checkIdsContained(ids)
      rm(list = ids, envir = self$env)
      invisible(self)
    },

    # [charvec] --> X. restricts to some elements from the dict
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
      ok = ids %chin% allids
      j = which.first(!ok)
      if (length(j) > 0L)
        gstop("{self$eltype} with id '{ids[j]}' not found!")
    },

    # deep-clones each element in env
    deep_clone = function(name, value) {
      if (name == "env")
        list2env(eapply(value, clone, deep = TRUE))
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


