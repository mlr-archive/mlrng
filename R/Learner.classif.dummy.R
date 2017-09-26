#' @include Learners.R
Learners$add(Learner$new(
  type = "classif",
  name = "dummy",
  par.set = makeParamSet(
    makeDiscreteParam("method", values = c("mode", "sample"), default = "mode")
  ),
  par.vals = list(),
  properties = c("missings", "factors", "numerics"),
  train = function(task, subset, data, ...) {
    tn = task$target
    mod = data[, .N, by = tn]
    class(mod) = c("dummy.model", class(mod))
    mod
  },

  predict = function(model, task, subset, data, method = "mode", ...) {
    if (method == "mode")
      rep.int(as.character(sample(model[N == max(N)][[task$target]], 1L)), nrow(data))
    else
      as.character(sample(model[[task$target]], nrow(data), replace = TRUE, prob = model[["N"]]))
  }
))
