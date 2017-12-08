#' @import checkmate
#' @import data.table
#' @import evaluate
#' @importFrom BBmisc vlapply viapply vcapply vnapply seq_row seq_col isFALSE
#' @importFrom digest digest
#' @importFrom fastmatch fmatch %fin%
#' @importFrom glue glue double_quote
#' @importFrom parallelMap parallelMap parallelExport parallelLibrary
#' @importFrom prettyunits pretty_sec
#' @importFrom R6 R6Class
#' @importFrom stats setNames predict
#' @importFrom utils data
#' @import phng
#' @import stringi
#' @keywords internal
"_PACKAGE"


mlrng = new.env(parent = emptyenv())
mlrng$supported.col.types = c(
  "logical", "integer", "numeric", "character", "factor", "ordered"
)
mlrng$supported.learner.props = c(
  sprintf("feat.%s", mlrng$supported.col.types),
  "missings", "weights", "twoclass", "multiclass", "prob", "featimp", "parallel",
  "formula", "oobpreds", "se"
)
mlrng$result.states = c(
  "Result", "TrainResult", "PredictResult", "PerformanceResult", "ResampleResult", "BenchmarkResult"
)
mlrng$default.opts = list(
  mlrng.verbose = TRUE,
  mlrng.debug = TRUE,
  mlrng.keep.train.output = FALSE,
  mlrng.continue.on.learner.error = FALSE,
  mlrng.train.encapsulation = 1L
)

.onLoad = function(libname, pkgname) { #nocov start
  backports::import(pkgname)
  backports::import(pkgname, "hasName", force = TRUE)
  parallelMap::parallelRegisterLevels(package = "mlrng", levels = c("resample", "benchmark", "tune"))

  # set default options
  opts = options()
  unset = which(!(names(mlrng$default.opts) %in% names(opts)))
  if (length(unset))
    options(mlrng$default.opts[unset])

  # read config file
  fn = file.path(rappdirs::user_config_dir("mlrng"), "config.yml")
  if (file.exists(fn)) {
    opts = try(yaml::read_yaml(fn))
    if (inherits(opts, "try-error") || !testList(opts, names = "unique")) {
      gwarn("Config file {fn} seems to be syntactically invalid")
    } else {
      names(opts) = sprintf("mlrng.%s", names(opts))
      options(opts)
    }
  }

} #nocov end

.onUnload = function(libpath) { #nocov start
  pm.opts = getOption("parallelMap.registered.levels")
  pm.opts$mlrng = NULL
  options(parallelMap.registered.levels = pm.opts)
} #nocov end
