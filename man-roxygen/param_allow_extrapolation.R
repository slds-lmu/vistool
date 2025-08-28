#' @param allow_extrapolation (`logical(1)`)
#'   Whether to evaluate the objective outside its evaluation (canonical) bounds when plot limits
#'   extend further. If `FALSE` (default), values outside are masked (set to `NA`) producing blank
#'   regions in the plot; if `TRUE`, the objective is called on the extended region (may yield
#'   misleading or numerically extreme values depending on the function definition).
