Backend = R6Class("Backend",
  public = list(
    rowid.col = NULL,
    writeable = NULL,
    transformators = list()
  ),
  private = list(
    transform = function(data) {
      nms = intersect(names(self$transformators), names(data))
      for (n in nms)
        set(data, j = n, value = self$transformators[[n]](data[[n]]))
      data
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
