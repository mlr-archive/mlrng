
#FIXME: we should also do all syntactic sugar for auto-arg conversions here


assertTask = function(task, subclass = "Task") {
  assert_r6(task, subclass)
}


assertLearner = function(learner, subclass = "Learner", for.task = NULL) {
  assert_r6(learner, subclass)
  if (!is.null(for.task)) {
    if (learner$type  != for.task$type)
      gstop("Types do not match: learner id={learner$id}, type={learner$type} vs task id={for.task$id}, type={for.task$type}")
  }
  invisible(learner)
}


assertIndexSet = function(subset, for.task = NULL) {
  #FIXME: can this be char log or int?
  #FIXME: translateSubset should probably go in here?
  # assertCharacter(subset, any.missing = FALSE, min.len = 1L)
  if (!is.null(for.task)) {
    #FIXME: check that row ids are valid for task
  }
  invisible(subset)
}


assertMeasures = function(measures, for.task = NULL) {
  assertList(measures, "Measure")
  if (!is.null(for.task)) {
    for (m in measures)
    if (for.task$type %nin% m$task.types)
      gstop("Types do not match: measure id={m$id}, types={collapse(m$task.types, ",")} vs task id={for.task$id}, type={for.task$type}")
  }
  invisible(measures)
}
