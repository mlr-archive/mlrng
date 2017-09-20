#' @export
Resampling = R6Class("Resampling",
  public = list(
    id = NA_character_,
    description = NA_character_,
    instantiate = NULL,
    iters = NA_integer_,
    checksum = NA_character_,
    pars = list(),
    instance = NULL,

    initialize = function(id, description, instantiate, iters, pars = list()) {
      self$id = assertString(id)
      self$description = assertString(description)
      if (!is.null(instantiate)) {
        self$instantiate = assertFunction(instantiate, args = "x")
        environment(self$instantiate) = environment(self$initialize)
      }
      self$iters = assertCount(iters)
      self$pars = assertList(pars, names = "unique")
      invisible(self)
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
    },

    set = function(train, test = list(NULL)) {
      self$instance = Map(Split$new, train = train, test = test)
      self$checksum = digest(self$instance, algo = "murmur32")
      invisible(self)
    },

    reset = function() {
      self$instance = NULL
      self$checksum = NA_character_
      invisible(self)
    }
  )
)

#' @export
as.data.table.Resampling = function(x, keep.rownames = FALSE, ...) {
  data.table(
    iter = seq_along(x),
    split = x$instance
  )
}
