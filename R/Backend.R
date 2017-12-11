Backend = R6Class("Backend",
  public = list(
    rowid.col = NULL,
    view.rows = NULL,
    view.cols = NULL,
    transformators = list()
  ),
  active = list(
    types = function() {
      vcapply(self$head(1L), class)
    }
  ),
  private = list(
    transform = function(data) {
      nms = intersect(names(self$transformators), names(data))
      for (n in nms)
        set(data, j = n, value = self$transformators[[n]](data[[n]]))
      data
    },

    deep_clone = function(name, value) {
      if (name == "view.rows") {
        if (is.null(value)) NULL else copy(value)
      } else if (name == "con") {
        NULL
      } else {
        value
      }
    }
  )
)

getDefaultTransformators = function(data) {
  getTrafo = function(x) {
    switch(class(x),
      character = function(x) as.character(x),
      factor = function(x) factor(x),
      integer = function(x) as.integer(x),
      logical = function(x) as.logical(x),
      NULL
    )
  }
  filterNull(lapply(data, getTrafo))
}
