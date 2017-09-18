#' @include Measures.R
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
is.Measure = function(x) {
  inherits(x, "Measure")
}
