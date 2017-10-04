getTaskData = function(task, subset = task$view$active.rows, type = "train", props = NULL) {
  assertR6(task, "TaskSupervised")
  assertAtomicVector(subset, any.missing = FALSE)
  assertChoice(type, choices = c("train", "test", "extra"))
  x = task$data(rows = subset, cols = task$features)
  convertFeatures(x, props)

  switch(type,
    train = set(x, j = task$target, value = task$data(subset, task$target))[],
    test = x,
    extra = list(y = task$data(subset, task$target), x = x)
  )
}

convertFeatures = function(x, props = NULL) {
  assertDataTable(x)
  if (!is.null(props)) {
    assertSubset(props, mlrng$supported.learner.props)
    mutate_if = function(x, predicate, conv, ...) {
      predicate = match.fun(predicate)
      nn = names(which(vlapply(x, predicate)))
      if (length(nn)) {
        conv = match.fun(conv)
        for (j in nn) set(x, j = j, value = conv(x[[j]], ...))
      }
    }

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
