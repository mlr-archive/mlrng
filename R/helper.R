requireNS = function(pkgs) {
  ok = vlapply(pkgs, requireNamespace, quietly = TRUE)
  if (!all(ok)) {
    gstop('Please install missing packages with
      install.packages({stri_flatten(double_quote(pkgs[!ok]), ",")})
    ', .call = FALSE)
  }
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
    return(task$backend$rownames)
  if (is.logical(subset)) {
    assertLogical(subset, len = task$nrow, any.missing = FALSE)
  } else {
    assertIntegerish(subset, any.missing = FALSE, lower = 1L, upper = task$nrow)
  }
  task$backend$rownames[subset]
}

createFallbackLearner = function(task) {
  mlr.learners$get(stri_paste(task$task.type, ".dummy"))
}

`%nin%` = function(x, y) {
  !match(x, y, nomatch = 0L)
}

`%fnin%` = function(x, y) {
  !fmatch(x, y, nomatch = 0L)
}

plural = function(n) {
  if (n != 1L) "s" else ""
}

copy_env = function(ee) {
  list2env(as.list(ee), parent = emptyenv())
}

filterNull = function(x) {
  x[!vlapply(x, is.null)]
}
