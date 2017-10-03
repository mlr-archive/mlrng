# p = probabilites for levs[2] => matrix with probs for levs[1] and levs[2]
propVectorToMatrix = function(p, levs) {
  assertNumeric(p)
  y = cbind(1 - p, p)
  colnames(y) = levs
  return(y)
}

# FIXME: do we want to keep the recode.target stuff from the old mlr?
getTaskData = function(task, subset = task$all.rows(), target.extra = FALSE) {
  if (target.extra) {
    # FIXME: If we allow user-defined formula, we might want to use task$formula to create data
    data = task$data(subset, task$features)
    data = convertDataFrameCols(data, chars.as.factor = TRUE, logicals.as.factor = TRUE)
    # FIXME: does this work with survival etc.?
    # FIXME: should target be a data.frame or vector?
    target = task$data(subset, task$target)
    target = convertDataFrameCols(target, chars.as.factor = TRUE, logicals.as.factor = TRUE)
    res = list(data = data, target = target[[task$target]])
    return(res)
  } else {
    data = task$data(subset)
    data = convertDataFrameCols(data, chars.as.factor = TRUE, logicals.as.factor = TRUE)
    return(data)
  }
}

