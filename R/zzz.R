#' @import checkmate
#' @import data.table
#' @import stringi
#' @import phng
#' @importFrom R6 R6Class
#' @importFrom stats setNames predict
NULL

# environment which holds constants and allows for reflections
mlrng = new.env(parent = emptyenv())

mlrng$default.opts = list(
  mlrng.verbose = TRUE,
  mlrng.debug = FALSE,
  mlrng.keep.train.output = FALSE,
  mlrng.continue.on.learner.error = FALSE
)

.onLoad = function(libname, pkgname) { #nocov start
  utils::globalVariables(c("role"), package = "mlrng")

  backports::import(pkgname)
  backports::import(pkgname, "hasName", force = TRUE)

  # set default + config options if not already set in this session
  opts = insert(mlrng$default.opts, readMlrngConfig())
  opts = opts[match(names(opts), names(.Options), nomatch = 0L) == 0L]
  if (length(opts))
    options(opts)
} #nocov end
