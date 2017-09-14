# Hooks:
# * pre.train: function(task, learner, subset)
# * post.train: function(task, model, learner, subset)
# * pre.predict: function(task, model, learner, subset)
# * post.predict: function(task, model, learner, pred, subset)
runHook = function(ee, hooks, name) {
  # filter for relevant hooks
  hooks = hooks[vlapply(hooks, hasName, name = name)]

  for (hook in hooks) {
    fun = hook[[name]]
    res = do.call(fun, c(as.list(ee), list(pars = hook[["pars"]])))
    for (name in names(res))
      assign(name, res[[name]], envir = ee)
  }
}
