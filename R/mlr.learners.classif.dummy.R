#' @include Dictionaries.R

mlr.learners$add(LearnerClassif$new(
  name = "dummy",
  par.set = ParamSetFlat$new(
    params = list(
      ParamCategorical$new("method", values = c("mode", "sample"), default = "mode")
    )
  ),
  par.vals = list(),
  properties = c("missings", "feat.factor", "feat.numeric"),

  train = function(task, subset, ...) {
    data = task$get(subset)
    tn = task$target
    mod = data[, .N, by = tn]
    class(mod) = c("dummy.model", class(mod))
    mod
  },

  predict = function(model, newdata, method = "mode", ...) {
    if (method == "mode")
      rep.int(as.character(sample(model$rmodel[N == max(N)][[model$task$target]], 1L)), nrow(newdata))
    else
      as.character(sample(model$rmodel[[model$task$target]], nrow(newdata), replace = TRUE, prob = model$rmodel[["N"]]))
  }
))
