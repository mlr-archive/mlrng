# evals multiple measures on pred, returns named numvec
performance = function(pred, measures) {
  perfs = vnapply(measures, function(x) x$fun(pred$truth, pred$response))
  mids = ids(measures)
  setNames(perfs, mids)
}
