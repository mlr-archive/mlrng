Param = R6Class("NewParam",
  public = list(
    data = NULL,
    initialize = function(id, type, len = 1L, lower = NULL, upper = NULL, default = NULL, values = NULL, trafo = NULL) {
      types = c("logical", "integer", "numeric", "character")
      self$data = list(
        id = assertString(id),
        type = factor(assertChoice(type, types), levels = types),
        len = assertCount(len),
        lower = as.double(assertNumber(lower, null.ok = TRUE)),
        upper = as.double(assertNumber(upper, null.ok = TRUE)),
        default = list(default),
        values = list(assertCharacter(values, unique = TRUE, min.chars = 1L, null.ok = TRUE)),
        trafo = list(assertFunction(trafo, null.ok = TRUE))
      )
    }
  )
)

ParamSet = R6Class("NewParamSet",
  public = list(
    data = NULL,
    initialize = function(...) {
      self$data = rbindlist(lapply(list(...), "[[", "data"), fill = TRUE)
      setkeyv(self$data, "id")
    },

    print = function() {
      print(self$data)
    },

    slice = function(ids) {
      self$data = self$data[ids]
      self
    },

    drop = function(ids) {
      self$data = self$data[!ids]
      self
    },

    filterType = function(x) {
      self$data[type %chin% x, "id"][[1L]]
    }
  )
)

#' @export
`[.NewParamSet` = function(x, i, j, ...) {
  y = x$clone()
  y$slice(i)
}

if (FALSE) {
  ps = ParamSet$new(
    Param$new("minsplit", "integer", lower = 0, upper = Inf, default = 20L),
    Param$new("cp", "numeric", lower = 0L, upper = 1)
  )
  par.set = ps
  address(ps$data)
  y = ps
  address(y$data)
  z = ps$clone()
  address(z$data)
  z$slice("cp")
  address(z$data)

  new.ps = ps[c("minsplit", "cp")]
  new.ps
  new.ps = ps["cp"]
  new.ps
  ps[ps$filterType("integer")]

  ps
}
