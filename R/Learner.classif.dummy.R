#' @include Learner.R
Learners$add(Learner$new(
  type = "classif",
  name = "dummy",
  par.set = makeParamSet(
    makeDiscreteParam("method", values = c("mode", "sample"), default = "mode")
  ),
  par.vals = list(),
  properties = c("missings", "factors", "numerics"),
  train = function(task, subset, ...) {
    tn = task$target
    mod = task[subset, tn][, .N, by = tn]
    class(mod) = c("dummy.model", class(mod))
    mod
  },

  predict = function(model, task, subset, method = "mode", ...) {
    if (method == "mode")
      rep.int(as.character(sample(model[N == max(N)][[task$target]], 1L)), length(subset))
    else
      as.character(sample(model[[task$target]], length(subset), replace = TRUE, prob = model[["N"]]))
  }
))
