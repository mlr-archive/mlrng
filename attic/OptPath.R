OptPath = R6Class("NewOptPath",
  public = list(
    initialize = function(par.set) {
      private$tab = data.table(
        dob = integer(0L),
        eol = integer(0L),
        msg = character(0L),
        exec.time = double(0L),
        extra = list())
      Map(function(id, type) {
        set(private$tab, j = id, value = get(type, mode = "function")())
      }, id = par.set$data$id, type = as.character(par.set$data$type))
      set(private$tab, j = "y", value = numeric(0L))
    },

    add = function(x, y, dob = NULL, eol = NA_integer_, msg = NA_character_, exec.time = NA_real_, extra = NULL) {
      if (private$cache.pos == length(private$cache))
        self$flush()

      cache.pos = private$cache.pos = private$cache.pos + 1L
      private$cache[[cache.pos]] = c(x, list(y = y, dob = dob %??% nrow(private$tab) + cache.pos,
          eol = eol, msg = msg, exec.time = exec.time, extra = list(extra)))
    },

    flush = function() {
      if (private$cache.pos > 0L) {
        cached = rbindlist(head(private$cache, private$cache.pos), fill = TRUE)
        private$tab = rbindlist(list(private$tab, cached), fill = TRUE)
        setorderv(private$tab, "dob")
        private$cache.pos = 0L
      }
    }
  ),

  active = list(
    data = function() {
      self$flush()
      private$tab
    }
  ),

  private = list(
    cache.pos = 0L,
    cache = vector("list", 512L),
    tab = NULL
  )
)

if (FALSE) {

  par.set = ParamSet$new(
    Param$new("minsplit", "integer", lower = 0, upper = Inf, default = 20L),
    Param$new("cp", "numeric", lower = 0L, upper = 1)
  )
  op = OptPath$new(par.set)
  op$data
  op$add(list(minsplit = 2, cp = 0.2), y = 18)
  op$add(list(minsplit = 10, cp = 0.2), y = 13)
  op$data
  system.time({
    for (i in 1:1e5) op$add(list(minsplit = 10, cp = 0.2), y = 13)
  })
  op$data
}
