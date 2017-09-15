#' @include Learner.R
Learners$add(Learner$new(
  type = "regr",
  name = "dummy",
  par.set = makeParamSet(
    makeDiscreteParam("method", values = c("mean", "median"), default = "mean")
  ),
  par.vals = list(),
  properties = c("missings", "factors", "numerics"),
  train = function(task, subset, method = "mean", ...) {
    tn = task$target
    mod = switch(method,
      "mean" = mean(x),
      "median" = median(x),
      stop("Illegal value for 'method'"))
    class(mod) = c("dummy.model", class(mod))
    mod
  },

  predict = function(model, task, subset, ...) {
    as.numeric(model)
  }
))
