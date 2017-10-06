#' @include Dictionaries.R

mlr.learners$add(LearnerClassif$new(
  name = "xgboost",
  package = "xgboost",
  par.set = makeParamSet(
    # we pass all of what goes in 'params' directly to ... of xgboost
    # makeUntypedLearnerParam(id = "params", default = list()),
    makeDiscreteLearnerParam(id = "booster", default = "gbtree", values = c("gbtree", "gblinear", "dart")),
    makeUntypedLearnerParam(id = "watchlist", default = NULL, tunable = FALSE),
    makeNumericLearnerParam(id = "eta", default = 0.3, lower = 0, upper = 1),
    makeNumericLearnerParam(id = "gamma", default = 0, lower = 0),
    makeIntegerLearnerParam(id = "max_depth", default = 6L, lower = 1L),
    makeNumericLearnerParam(id = "min_child_weight", default = 1, lower = 0),
    makeNumericLearnerParam(id = "subsample", default = 1, lower = 0, upper = 1),
    makeNumericLearnerParam(id = "colsample_bytree", default = 1, lower = 0, upper = 1),
    makeNumericLearnerParam(id = "colsample_bylevel", default = 1, lower = 0, upper = 1),
    makeIntegerLearnerParam(id = "num_parallel_tree", default = 1L, lower = 1L),
    makeNumericLearnerParam(id = "lambda", default = 1, lower = 0),
    makeNumericLearnerParam(id = "lambda_bias", default = 0, lower = 0),
    makeNumericLearnerParam(id = "alpha", default = 0, lower = 0),
    makeUntypedLearnerParam(id = "objective", default = "binary:logistic", tunable = FALSE),
    makeUntypedLearnerParam(id = "eval_metric", default = "error", tunable = FALSE),
    makeNumericLearnerParam(id = "base_score", default = 0.5, tunable = FALSE),
    makeNumericLearnerParam(id = "max_delta_step", lower = 0, default = 0),
    makeNumericLearnerParam(id = "missing", default = NULL, tunable = FALSE, when = "both",
      special.vals = list(NA, NA_real_, NULL)),
    makeIntegerVectorLearnerParam(id = "monotone_constraints", default = 0, lower = -1, upper = 1),
    makeNumericLearnerParam(id = "tweedie_variance_power", lower = 1, upper = 2, default = 1.5, requires = quote(objective == "reg:tweedie")),
    makeIntegerLearnerParam(id = "nthread", lower = 1L, tunable = FALSE),
    makeIntegerLearnerParam(id = "nrounds", default = 1L, lower = 1L),
    # FIXME nrounds seems to have no default in xgboost(), if it has 1, par.vals is redundant
    makeUntypedLearnerParam(id = "feval", default = NULL, tunable = FALSE),
    makeIntegerLearnerParam(id = "verbose", default = 1L, lower = 0L, upper = 2L, tunable = FALSE),
    makeIntegerLearnerParam(id = "print_every_n", default = 1L, lower = 1L, tunable = FALSE,
      requires = quote(verbose == 1L)),
    makeIntegerLearnerParam(id = "early_stopping_rounds", default = NULL, lower = 1L, special.vals = list(NULL), tunable = FALSE),
    makeLogicalLearnerParam(id = "maximize", default = NULL, special.vals = list(NULL), tunable = FALSE),
    makeDiscreteLearnerParam(id = "sample_type", default = "uniform", values = c("uniform", "weighted"), requires = quote(booster == "dart")),
    makeDiscreteLearnerParam(id = "normalize_type", default = "tree", values = c("tree", "forest"), requires = quote(booster == "dart")),
    makeNumericLearnerParam(id = "rate_drop", default = 0, lower = 0, upper = 1, requires = quote(booster == "dart")),
    makeNumericLearnerParam(id = "skip_drop", default = 0, lower = 0, upper = 1, requires = quote(booster == "dart")),
    # TODO: uncomment the following after the next CRAN update, and set max_depth's lower = 0L
    #makeLogicalLearnerParam(id = "one_drop", default = FALSE, requires = quote(booster == "dart")),
    #makeDiscreteLearnerParam(id = "tree_method", default = "exact", values = c("exact", "hist"), requires = quote(booster != "gblinear")),
    #makeDiscreteLearnerParam(id = "grow_policy", default = "depthwise", values = c("depthwise", "lossguide"), requires = quote(tree_method == "hist")),
    #makeIntegerLearnerParam(id = "max_leaves", default = 0L, lower = 0L, requires = quote(grow_policy == "lossguide")),
    #makeIntegerLearnerParam(id = "max_bin", default = 256L, lower = 2L, requires = quote(tree_method == "hist")),
    makeUntypedLearnerParam(id = "callbacks", default = list(), tunable = FALSE)
  ),
  par.vals = list(nrounds = 1L, verbose = 0L),
  properties = c("twoclass", "multiclass", "feat.numeric", "prob", "weights", "missings", "featimp"),
  
  train = function(task, subset, weights = NULL, ...) {
    nc = task$nclasses
    parlist = list(...)
    
    if (is.null(parlist$objective))
      parlist$objective = ifelse(nc == 2L, "binary:logistic", "multi:softprob")
    
    if (self$predict.type == "prob" && parlist$objective == "multi:softmax")
      stop("objective = 'multi:softmax' does not work with predict.type = 'prob'")
    
    #if we use softprob or softmax as objective we have to add the number of classes 'num_class'
    if (parlist$objective %in% c("multi:softprob", "multi:softmax"))
      parlist$num_class = nc
    
    data = getTaskData(task, subset = subset, type = "train", target.as = "factor", props = self$properties)
    d = BBmisc::dropNamed(data, drop = task$target)
    truth = task$truth()
    label = match(as.character(truth[[task$target]]), task$classes) - 1
    parlist$data = xgboost::xgb.DMatrix(data = data.matrix(d), label = data.matrix(label))
    
    if (!is.null(weights))
      xgboost::setinfo(parlist$data, "weight", weights)
    
    if (is.null(parlist$watchlist))
      parlist$watchlist = list(train = parlist$data)
    
    do.call(xgboost::xgb.train, parlist)
  },
  
  predict = function(model, newdata, ...) {
    cl = model$task$classes
    nc = model$task$nclasses
    obj = self$par.vals$objective
    
    if (is.null(obj))
      self$par.vals$objective = ifelse(nc == 2L, "binary:logistic", "multi:softprob")
    
    p = predict(model$rmodel, newdata = data.matrix(newdata), ...)
    
    if (nc == 2L) { #binaryclass
      if (self$par.vals$objective == "multi:softprob") {
        y = matrix(p, nrow = length(p) / nc, ncol = nc, byrow = TRUE)
        colnames(y) = cl
      } else {
        y = matrix(0, ncol = 2, nrow = nrow(newdata))
        colnames(y) = cl
        y[, 1L] = 1 - p
        y[, 2L] = p
      }
      if (self$predict.type == "prob") {
        unname(y)
      } else {
        p = colnames(y)[max.col(y)]
        names(p) = NULL
        p = factor(p, levels = colnames(y))
        unname(p)
      }
    } else { #multiclass
      if (self$par.vals$objective  == "multi:softmax") {
        unname(factor(p, levels = cl)) #special handling for multi:softmax which directly predicts class levels
      } else {
        p = matrix(p, nrow = length(p) / nc, ncol = nc, byrow = TRUE)
        colnames(p) = cl
        if (self$predict.type == "prob") {
          unname(p)
        } else {
          ind = max.col(p)
          cns = colnames(p)
          unname(factor(cns[ind], levels = cns))
        }
      }
    }
  },
  
  model.extractors = list(
    featureImportance = function(model, task, subset, ...) {
      mod = model$rmodel
      imp = xgboost::xgb.importance(feature_names = model$task$features,
        model = mod, ...)
      fiv = imp$Gain
      setNames(fiv, imp$Feature)
    }
  )
))
