requireNS = function(x) {
  ok = vlapply(x, requireNamespace, quietly = TRUE)
  if (!all(ok))
    stop("Please install the following packages: ", stri_flatten(x[!ok], ", "))
}

gcat = function(..., .sep = "", .envir = parent.frame(), .file = "", .append = TRUE) {
  cat(glue(..., .sep = .sep, .envir = .envir), "\n", file = .file, append = .append)
}

gmessage = function(..., .sep = "", .envir = parent.frame()) {
  message(glue(..., .sep = .sep, .envir = .envir), appendLF = TRUE)
}

ginfo = function(..., .sep = "", .envir = parent.frame()) {
  if (getOption("mlrng.verbose", FALSE))
    message(glue(..., .sep = .sep, .envir = .envir), appendLF = TRUE)
}

gwarn = function(..., .sep = "", .envir = parent.frame(), .call = TRUE) {
  warning(glue(..., .sep = .sep, .envir = .envir), .call = .call, noBreaks. = TRUE)
}

gstop = function(..., .sep = "", .envir = parent.frame(), .call = TRUE) {
  msg = glue(..., .sep = .sep, .envir = .envir)
  if (.call)
    stop(simpleError(msg, call = sys.call(sys.parent())))
  else
    stop(msg, call. = FALSE)
}

ids = function(x) {
  vcapply(x, function(x) x$id)
}

stri_peek = function(str, append = "...") {
  if (length(str) == 0L)
    return("")
  width = getOption("width", 80L)
  str = stri_flatten(str, ",")
  if (stri_length(str) > width)
    return(stri_join(stri_sub(str, 1L, width - stri_length(append)), append))
  return(str)
}

stri_pasteNames = function(str, sep = " ", collapse = ", ", names.first = TRUE) {
  if(names.first)
    stri_paste(names(str), str, sep = sep, collapse = collapse)
  else
    stri_paste(str, names(str), sep = sep, collapse = collapse)
}

translateSubset = function(task, subset = NULL) {
  if (is.null(subset))
    return(task$view$active.rows)
  if (is.logical(subset)) {
    assertLogical(subset, len = task$nrow, any.missing = FALSE)
  } else {
    assertIntegerish(subset, any.missing = FALSE, lower = 1L, upper = task$nrow)
  }
  task$view$active.rows[subset]
}


asListOfRows = function(x) {
  if (is.vector(x)) {
    as.list(x)
  } else if (is.matrix(x)) {
    apply(x, 1, list)
  } else if (is.data.frame(x)) {
    if (ncol(x) == 1L)
      as.list(x[[1L]])
    else
      .mapply(list, x, MoreArgs = list())
  }
}

createFallbackLearner = function(task) {
 mlr.learners$get(stri_paste(task$task.type, ".dummy"))
}

`%nin%` = function(x, y) {
  !match(x, y, nomatch = 0L)
}

`%chnin%` = function(x, y) {
  !chmatch(x, y, nomatch = 0L)
}

plural = function(n) {
  if (n != 1L) "s" else ""
}

copy_env = function(ee) {
  list2env(as.list(ee), parent = emptyenv())
}
