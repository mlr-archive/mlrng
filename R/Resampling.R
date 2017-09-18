#' @include Resamplings.R
#' @export
Resampling = R6Class("Resampling",
  public = list(
    id = NA_character_,
    description = NA_character_,
    instantiate = NULL,
    iters = NA_integer_,
    pars = list(),
    instance = NULL,

    initialize = function(id, description, instantiate, iters, pars = list()) {
      self$id = assertString(id)
      self$description = assertString(description)
      self$instantiate = assertFunction(instantiate, args = "x")
      self$iters = assertCount(iters)
      self$pars = assertList(pars, names = "unique")
      environment(self$instantiate) = environment(self$initialize)
    },

    reset = function() {
      self$instance = NULL
    },

    train = function(i) {
      if (is.null(self$instance))
        stop("Resampling has not been instantiated yet")
      self$instance[[i]][["train"]]
    },

    test = function(i) {
      if (is.null(self$instance))
        stop("Resampling has not been instantiated yet")
      self$instance[[i]][["test"]]
    }
  ),

  private = list(
    setInstance = function(train, test = list(NULL)) {
      self$instance = Map(Split$new, train = train, test = test)
      invisible(self)
    }
  )
)

#' @export
length.Resampling = function(x) {
  x$iters
}

#' @export
`[[.Resampling` = function(x, i, ...) {
  if (is.null(x$instance))
    stop("Resampling has not been instantiated yet")
  assertInt(i, lower = 1L, upper = x$iters)
  x$instance[[i]]
}
