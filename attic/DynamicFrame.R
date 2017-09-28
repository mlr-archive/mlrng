DynamicFrame = R6::R6Class("DynamicFrame",
  public = list(
    initialize = function(size = 128L) {
      assertCount(size)
      private$cache = vector("list", size)
    },

    add = function(row) {
      if (private$cache.pos == length(private$cache))
        self$flush(TRUE)

      i = private$cache.pos = private$cache.pos + 1L
      private$cache[[i]] = row
    },

    flush = function(increase.cache.size = FALSE) {
      if (private$cache.pos > 0L) {
        bound = rbindlist(head(private$cache, private$cache.pos), fill = TRUE)
        private$tab = rbind(private$tab, bound, fill = TRUE)
        private$cache.pos = 0L
        if (increase.cache.size)
          private$cache = vector("list", 2 * length(private$cache))
        invisible(self)
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
    cache = NULL,
    tab = NULL
  )
)

x = DynamicFrame$new(3)
x$add(list(a = 1, b = 2))
x$add(list(a = 2, b = 9))
x$data
