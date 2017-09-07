#' @include Register.R
Measures = Register$new("Measure")

Measure = R6Class("Measure",
  public = list(
    id = NA_character_,
    requires = character(0L),
    fun = NULL,
    initialize = function(id, requires, fun) {
      self$id = id
      self$requires = requires
      self$fun = fun
    }
  )
)

assertMeasure = function(x) {
  assertClass(x, "Measure")
}

getMeasure = function(x) {
  Measures$get(x)
}

getMeasures = function(x) {
  if (!is.list(x))
    x = list(x)
  res = lapply(x, Measures$get)
  setNames(res, ids(res))
}

#' @export
listMeasures = function() {
  tab = rbindlist(eapply(Measures$storage, function(x) {
    list(
      id = x$id,
      requires = list(x$requires)
    )
  }, USE.NAMES = FALSE))
  setkeyv(tab, "id")[]
}
