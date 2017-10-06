#' @include Dictionaries.R

mlr.learners$add(LearnerRegr$new(
  name = "ranger",
  package = "ranger",
  par.set = makeParamSet(
    makeIntegerLearnerParam(id = "num.trees", lower = 1L, default = 500L),
    # FIXME: Add default value when data dependent defaults are implemented: mtry=floor(sqrt(#independent vars))
    makeIntegerLearnerParam(id = "mtry", lower = 1L),
    makeIntegerLearnerParam(id = "min.node.size", lower = 1L, default = 5L),
    makeLogicalLearnerParam(id = "replace", default = TRUE),
    makeNumericLearnerParam(id = "sample.fraction", lower = 0L, upper = 1L),
    makeNumericVectorLearnerParam(id = "split.select.weights", lower = 0, upper = 1),
    makeUntypedLearnerParam(id = "always.split.variables"),
    makeLogicalLearnerParam(id = "respect.unordered.factors", default = FALSE),
    makeDiscreteLearnerParam(id = "importance", values = c("none", "impurity", "permutation"), default = "none", tunable = FALSE),
    makeLogicalLearnerParam(id = "write.forest", default = TRUE, tunable = FALSE),
    makeLogicalLearnerParam(id = "scale.permutation.importance", default = FALSE, requires = quote(importance == "permutation"), tunable = FALSE),
    makeIntegerLearnerParam(id = "num.threads", lower = 1L, when = "both", tunable = FALSE),
    makeLogicalLearnerParam(id = "save.memory", default = FALSE, tunable = FALSE),
    makeLogicalLearnerParam(id = "verbose", default = TRUE, when = "both", tunable = FALSE),
    makeIntegerLearnerParam(id = "seed", when = "both", tunable = FALSE),
    makeDiscreteLearnerParam(id = "splitrule", values = c("variance", "maxstat"), default = "variance"),
    makeNumericLearnerParam(id = "alpha", lower = 0L, upper = 1L, default = 0.5, requires = quote(splitrule == "maxstat")),
    makeNumericLearnerParam(id = "minprop", lower = 0L, upper = 1L, default = 0.1, requires = quote(splitrule == "maxstat")),
    makeLogicalLearnerParam(id = "keep.inbag", default = FALSE, tunable = FALSE)
  ),
  par.vals = list(num.threads = 1L, verbose = FALSE, respect.unordered.factors = TRUE),
  properties = c("formula", "feat.numeric", "feat.factor", "feat.ordered", "oobpreds", "featimp", "parallel"),
  
  train = function(task, subset, ...) {
    data = getTaskData(task, subset = subset, type = "train", props = self$properties)
    ranger::ranger(formula = task$formula, data = data, ...)
  },
  
  predict = function(model, newdata, ...) {
    p = predict(object = model$rmodel, data = newdata, ...)
    unname(p$predictions)
  },
  
  model.extractors = list(
    OOBPredictions = function(model, task, subset, ...) {
      model$predictions
    },
    FeatureImportance = function(model, task, subset, ...) {
      has.fiv = self$par.vals$importance
      if (is.null(has.fiv) || has.fiv == "none") {
        stop("You must set the learners parameter value for importance to
          'impurity' or 'permutation' to compute feature importance")
      }
      ranger::importance(model)
    }
  )
))
