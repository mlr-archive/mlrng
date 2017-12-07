#' @field id [\code{character(1)}]: Identifier of the task.
#' @field backend [\code{Backend}]: Internal abstraction for data access. Leave this alone unless you know exactly what you are doing.
#' @field get [\code{function(rows = NULL, cols = NULL)}]: Names of the features.
#' @field features [\code{character}]: Names of the features.
#' @field formula [\code{character}]: Formula describing the learning task.
