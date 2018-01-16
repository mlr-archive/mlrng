getTaskData = function(task, row.ids, type = "train", props = NULL, target.as = NULL) {
  assertR6(task, "TaskSupervised")
  assertAtomicVector(row.ids, any.missing = FALSE)
  assertChoice(type, choices = c("train", "predict", "extra"))
  assertSubset(target.as, c("factor", "character"), empty.ok = TRUE)
  x = task$get(rows = row.ids, cols = task$features)
  convertFeatures(x, props)

  target = task$truth(row.ids)
  if (!is.null(target.as)) {
    if (target.as == "factor") {
      levs = task$backend$distinct(col = task$target)
      mutate_if(target, function(x) !is.factor(x), function(x) factor(x, levels = levs))
    } else if (target.as == "character") {
      mutate_if(target, function(x) !is.character(x), as.character)
    }
  }

  switch(type,
    train = cbind(target, x),
    predict = x,
    extra = list(y = target, x = x)
  )
}

convertFeatures = function(x, props = NULL) {
  assertDataTable(x)
  if (!is.null(props)) {
    assertSubset(props, mlrng$supported.learner.props, fmatch = TRUE)

    if ("feat.logical" %fnin% props)
      mutate_if(x, is.logical, as.integer)
    if ("feat.integer" %fnin% props)
      mutate_if(x, is.integer, as.double)
    if ("feat.character" %fin% props && "factors" %fnin% props)
      mutate_if(x, is.factor, as.character)
    if ("feat.factor" %fin% props && "characters" %fnin% props)
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
