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
  width = getOption("width", 80L)
  str = stri_flatten(str, ",")
  if (stri_length(str) > width)
    return(stri_join(stri_sub(str, 1L, width - stri_length(append)), append))
  return(str)
}

asSubset = function(x, subset = NULL) {
  if (inherits(x, "Task"))
    x = x$backend$nrow
  if (is.null(subset))
    return(!bit(x))
  if (is.logical(subset))
    return(as.bit(assertLogical(subset, len = x, any.missing = FALSE)))
  assertIntegerish(subset, any.missing = FALSE, lower = 1L, upper = x)
  replace(bit(x), subset, TRUE)
}
