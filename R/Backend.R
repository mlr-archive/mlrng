Backend = R6Class("Backend",
  public = list(
    rowid.col = NULL,
    mutators = list()
  ),
  private = list(
    mutate = function(data) {
      nms = intersect(names(self$mutators), names(data))
      for (n in nms)
        set(data, j = n, value = self$mutators[[n]](data[[n]]))
      data
    }
  )
)

getDefaultMutators = function(data) {
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
