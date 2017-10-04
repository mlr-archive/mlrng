# p = probabilites for levs[2] => matrix with probs for levs[1] and levs[2]
propVectorToMatrix = function(p, levs) {
  assertNumeric(p)
  y = cbind(1 - p, p)
  colnames(y) = levs
  return(y)
}

# FIXME: do we want to keep the recode.target stuf task$all.rows()f from the old mlr?
getTaskData = function(task, subset = task$view$active.rows, target.extra = FALSE) {
  if (target.extra) {
    # FIXME: If we allow user-defined formula, we might want to use task$formula to create data
    data = task$data(subset, setdiff(task$view$active.cols, task$target))
    data = BBmisc::convertDataFrameCols(data, chars.as.factor = TRUE, logicals.as.factor = TRUE)
    # FIXME: does this work with survival etc.?
    # FIXME: should target be a data.frame or vector?
    target = task$data(subset, task$target)
    target = BBmisc::convertDataFrameCols(target, chars.as.factor = TRUE, logicals.as.factor = TRUE)
    res = list(data = data, target = target[[task$target]])
    return(res)
  } else {
    data = task$data(subset, task$view$active.cols)
    data = BBmisc::convertDataFrameCols(data, chars.as.factor = TRUE, logicals.as.factor = TRUE)
    return(data)
  }
}

#' @title Convert arguments to control structure.
#'
#' @description
#' Find all elements in \code{...} which are not missing and
#' call \code{control} on them.
#'
#' @param control [\code{function}]\cr
#'   Function that creates control structure.
#' @param ... [any]\cr
#'   Arguments for control structure function.
#' @param .defaults [\code{list}]\cr
#'   Initial default values that the control function is called on.
#'   One can use this if control should be called on different defaults than in its signature.
#'   The values in ... will overwrite these.
#'   Default is empty list.
#' @param .restrict [\code{logical(1)}]\cr
#'   Restrict the elements from \code{...}, which are passed to \code{control}, only to the arg-names
#'   that occur in the signature of \code{control}?
#'   Default is \code{FALSE}.
#' @return Control structure for learner.
#' @export
learnerArgsToControl = function(control, ..., .defaults = list(), .restrict = FALSE) {
  if (.restrict)
    allowed.arg.names = names(formals(control))
  args = .defaults
  dots = match.call(expand.dots = FALSE)$...
  for (i in seq_along(dots)) {
    arg = dots[[i]]
    is.missing = if (is.symbol(arg)) {
      argname = as.character(arg)
      eval(substitute(missing(symbol), list(symbol = arg)),
        envir = parent.frame())
    } else {
      argname = names(dots)[i]
      FALSE
    }
    if (!is.missing && (!.restrict || argname %in% allowed.arg.names)) {
      value = tryCatch(eval(arg, envir = parent.frame()), error = function(...) NULL)
      if (!is.null(value)) {
        args[[as.character(argname)]] = value
      }
    }
  }
  do.call(control, args)
}
