getTaskData = function(task, subset = NULL, type = "train", props = NULL, target.as = NULL) {
  assertR6(task, "TaskSupervised")
  # subset = translateSubset(task, subset)
  assertAtomicVector(subset, any.missing = FALSE)
  # FIXME: Maybe we want also 'target.as' to be logical or integer, however beware of typecast errors.
  assertSubset(target.as, c("factor", "character"), empty.ok = TRUE)
  assertChoice(type, choices = c("train", "test", "extra"))
  x = task$get(rows = subset, cols = task$features)
  convertFeatures(x, props)

  target = task$truth(subset)
  if (!is.null(target.as)) {
    # FIXME: what happens with multilabel when we have multiple target-factors with different levels?
    if (target.as == "factor") {
      levs = task$backend$distinct(col = task$target)
      mutate_if(target, function(x) !is.factor(x), function(x) factor(x, levels = levs))
    } else if (target.as == "character") {
      mutate_if(target, function(x) !is.character(x), as.character)
    }
  }

  switch(type,
    train = cbind(target, x),
    test = x,
    extra = list(y = target, x = x)
  )
}

convertFeatures = function(x, props = NULL) {
  assertDataTable(x)
  if (!is.null(props)) {
    assertSubset(props, mlrng$supported.learner.props)

    if ("feat.logical" %chnin% props)
      mutate_if(x, is.logical, as.integer)
    if ("feat.integer" %chnin% props)
      mutate_if(x, is.integer, as.double)
    if ("feat.character" %chin% props && "factors" %chnin% props)
      mutate_if(x, is.factor, as.character)
    if ("feat.factor" %chin% props && "characters" %chnin% props)
      mutate_if(x, is.character, as.factor)
  }

  return(x)
}

mutate_if = function(x, predicate, conv, ...) {
  predicate = match.fun(predicate)
  nn = names(which(vlapply(x, predicate)))
  if (length(nn)) {
    conv = match.fun(conv)
    for (j in nn) set(x, j = j, value = conv(x[[j]], ...))
  }
}
