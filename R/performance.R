# evals multiple measures on pred, returns named numvec
performance = function(pred.res, measures) {
  assert_r6(pred.res, "PredictResult")
  assertMeasures(measures, for.task = pred.res$task)
  perf.vals = vnapply(measures, function(x) x$fun(pred.res))
  mids = ids(measures)
  perf.vals = setNames(perf.vals, mids)
  PerformanceResult$new(pred.res, measures, perf.vals)
}
