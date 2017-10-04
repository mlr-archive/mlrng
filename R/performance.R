# evals multiple measures on pred, returns named numvec
performance = function(pred.res, measures) {
  perf.vals = vnapply(measures, function(x) x$fun(pred.res))
  mids = ids(measures)
  perf.vals = setNames(perf.vals, mids)
  PerforemanceResult$new(pred.res, perf.vals)
}




