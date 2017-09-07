#' @include Register.R
Resamplings = Register$new("Resampling")

Resampling = R6Class("Resampling",
  public = list(
    id = NA_character_,
    description = NA_character_,
    instantiate = NULL,
    pars = list(),
    instance = NULL,

    initialize = function(id, description, instantiate, pars = list()) {
      self$id = assertString(id)
      self$description = assertString(description)
      self$instantiate = assertFunction(instantiate, args = "x")
      self$pars = assertList(pars, names = "unique")
    },
    reset = function() {
      self$instance = NULL
    }
  )
)

#' @export
length.Resampling = function(x) {
  x$pars$iters
}

getNestedResampling = function(n, outer, inner) {
  if (FALSE) {
    task = getTask("iris")
    n = task$nrow
    self = list(
      pars = list(
    outer = getResampling("cv"),
    inner = getResampling("holdout")
    ))
  }
  assertClass(outer, "Resampling")
  assertClass(inner, "Resampling")

  Resampling$new(
    id = "nested resampling",
    description = sprintf("nested resampling: [%s]x[%s]", outer$id, inner$id),
    instantiate = function(x) {
      if (inherits(x, "Task"))
        x = x$nrow
      assertCount(x)
      self$pars$outer$instantiate(x)
      ni = apply(self$pars$outer$instance, 2L, sum)
      instance = lapply(ni, function(n) self$pars$inner$clone()$instantiate(n))
      self$instance = do.call(cbind, instance)
    },
    pars = list(iters = 10L, inner = inner, outer = outer)
  )
}

getResampling = function(x, task = NULL) {
  rdesc = Resamplings$get(x)
  if (!is.null(task)) {
    task = getTask(task)
    if (is.null(rdesc$instance)) {
      rdesc = rdesc$clone()
      rdesc$instantiate(task)
    } else if (nrow(rdesc$instance) != task$nrow) {
      stop("Resampling has been instantiate with a different task size")
    }
  }
  return(rdesc)
}

getResamplings = function(x, tasks = NULL) {
  if (is.null(tasks))
    tasks = list(NULL)
  if (!is.list(x))
    x = list(x)
  res = Map(getResampling, x = x, task = tasks)
  setNames(res, ids(res))
}

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
  assertInt(i, lower = 1L, upper = ncol(x$instance))
  x$instance[, i]
}
