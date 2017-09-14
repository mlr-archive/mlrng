#' @include Register.R

#' @export
Measures = Register$new("Measure")

#' @export
Measure = R6Class("Measure",
  public = list(
    id = NA_character_,
    name = NA_character_,
    tasktypes = character(0L),
    requires = character(0L),
    fun = NULL,
    initialize = function(id, name, tasktypes, requires, fun) {
      self$id = id
      self$name = name
      self$requires = requires
      self$tasktypes = tasktypes
      self$fun = fun
    }
  )
)

#' @export
getMeasure = function(x) {
  Measures$get(x)
}

#' @export
getMeasures = function(x) {
  Measures$mget(x)
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
