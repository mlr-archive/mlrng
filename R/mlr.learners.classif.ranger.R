#' @include Dictionaries.R

mlr.learners$add(LearnerClassif$new(
  name = "ranger",
  package = "ranger",
  par.set = makeParamSet(
    makeIntegerLearnerParam(id = "num.trees", lower = 1L, default = 500L),
    # FIXME: Add default value when data dependent defaults are implemented: mtry=floor(sqrt(#independent vars))
    makeIntegerLearnerParam(id = "mtry", lower = 1L),
    # FIXME: Add default value when data dependent defaults are implemented: min.node.size = 1 for classification, 10 for probability prediction
    makeIntegerLearnerParam(id = "min.node.size", lower = 1L),
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
    makeLogicalLearnerParam(id = "keep.inbag", default = FALSE, tunable = FALSE)
  ),
  par.vals = list(num.threads = 1L, verbose = FALSE, respect.unordered.factors = TRUE),
  properties = c("twoclass", "multiclass", "prob", "feat.numeric", "feat.factor", "feat.ordered", "featimp", "weights", "parallel", "formula"),
  
  train = function(task, subset, weights = NULL, ...) {
    tn = task$target
    data = getTaskData(task, subset = subset, type = "train", target.as = "factor", props = self$properties)
    ranger::ranger(formula = task$formula, data = data, probability = (self$predict.type == "prob"),
      case.weights = weights, ...)
  },

  predict = function(model, newdata, ...) {
    pt = self$predict.type
    if (pt == "response") {
      p = predict(model$rmodel, data = newdata, type = "response", ...)
      return(as.character(p$predictions))
    } else { # FIXME: Probability estimation needs to be fixed
      p = predict(model$rmodel, data = newdata, predict.all = TRUE, ...)
      return(p$predictions)
    }
  },
  
  model.extractors = list( # FIXME: not working right now
    OOBPredictions = function(model, task = NULL, subset = NULL, ...) {
      model$predictions
    },
    featureImportance = function(model, task = NULL, subset = NULL, ...) { # FIXME: not working right now
      has.fiv = self$par.vals$importance
      if (is.null(has.fiv) || has.fiv == "none") {
        stop("You must set the parameter value for 'importance' to
        'impurity' or 'permutation' to compute feature importance.")
      }
      ranger::importance(model$rmodel)
    }
  )
))
