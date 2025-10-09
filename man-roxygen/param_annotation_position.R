#' @param position (`list()`|`NULL`)\cr
#'   Optional relative placement specification for the annotation. Provide a named list
#'   with entries `x`, `y`, `z` (depending on dimensionality) holding values in `[0, 1]`
#'   and an optional `reference` that is either `"panel"` (default, data coordinates)
#'   or `"figure"` (normalized figure coordinates). When `position` is supplied, do not
#'   pass absolute coordinates via `x`, `y`, or `z`.
