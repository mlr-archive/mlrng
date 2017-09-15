#' @include Dictionary.R

#' @export
Resamplings = Dictionary$new("Resampling")

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

    reset = function() self$instance = NULL,
    train = function(i) self$instance[[i]]$train,
    test = function(i) self$instance[[i]]$test
  ),

  private = list(
    setInstance = function(train, test = NULL) {
      self$instance = Map(Split$new, train = train, test = test %??% list(NULL))
    }
  )
)

Split = R6Class("Split",
  private = list(
    train.bit = NULL,
    test.bit = NULL
  ),
  public = list(
    initialize = function(train, test = NULL) {
      private$train.bit = as.bit(train)
      private$test.bit = as.bit(test %??% !private$train.bit)
    }
  ),
  active = list(
    train = function() as.which(private$train.bit),
    test = function() as.which(private$test.bit)
  )
)

#' @export
length.Resampling = function(x) {
  x$iters
}

# getNestedResampling = function(outer, inner) {
#   assertClass(outer, "Resampling")
#   assertClass(inner, "Resampling")

#   Resampling$new(
#     id = "nested resampling",
#     description = sprintf("nested resampling: [%s]x[%s]", outer$id, inner$id),
#     instantiate = function(x) {
#       if (inherits(x, "Task"))
#         x = x$nrow
#       assertCount(x)

#       self$pars$outer$instantiate(x)
#       no = length(self$pars$outer)
#       ni = length(self$pars$inner)
#       tmp = vector("list", no * ni)
#       self$instance = list(train = tmp, test = tmp)

#       for (i in seq_len(no)) {
#         train = self$pars$outer$instance$train[[i]]
#         self$pars$inner$instantiate(sum(train))
#         for (j in seq_len(ni)) {
#           ij = (i-1L) * ni + j
#           ind = as.bitwhich(train)
#           self$instance$train[[ij]] = replace(train, ind, self$pars$inner$instance$train[[j]])
#           self$instance$test[[ij]] = replace(train, ind, self$pars$inner$instance$test[[j]])
#         }
#       }
#     },
#     iters = length(inner) * length(outer),
#     pars = list(inner = inner, outer = outer)
#   )
# }


#' @export
listResamplings = function() {
  tab = rbindlist(eapply(Resamplings$storage, function(r) {
    list(
      id = r$id,
      description = r$description,
      pars = list(r$pars)
    )
  }, USE.NAMES = FALSE))
  setkeyv(tab, "id")[]
}

#' @export
`[[.Resampling` = function(x, i, ...) {
  if (is.null(x$instance))
    stop("Resampling has not been instantiated yet")
  assertInt(i, lower = 1L, upper = length(x$iters))
  x$train(i)
}
