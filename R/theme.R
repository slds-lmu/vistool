#' vistool theming utilities
#'
#' @description
#' Lightweight theme model and helpers to manage plotting style in a single place.
#'
#' @param palette Character. Color palette to use. One of "viridis", "plasma", or "grayscale".
#' @param text_size Numeric. Base text size for plots.
#' @param theme Character. ggplot2 theme to use. One of "minimal", "bw", "classic", "gray", "grey", "light", "dark", or "void".
#' @param alpha Numeric. Transparency level (0-1).
#' @param line_width Numeric. Default line width for plots.
#' @param point_size Numeric. Default point size for plots.
#' @param legend_position Character. Position of legend. One of "top", "right", "bottom", "left", or "none".
#' @param show_grid Logical. Whether to show grid lines.
#' @param grid_color Character. Color of grid lines.
#' @param background Character. Background color of plots.
#'
#' @examples
#' th = vistool_theme(palette = "plasma", text_size = 12)
#' @export
vistool_theme = function(
    palette = "viridis",
    text_size = 11,
    theme = "minimal",
    alpha = 0.8,
    line_width = 1.2,
    point_size = 2,
    legend_position = "right",
    show_grid = TRUE,
    grid_color = "gray90",
    background = "white") {
  th = list(
    palette = palette,
    text_size = text_size,
    theme = theme,
    alpha = alpha,
    line_width = line_width,
    point_size = point_size,
    legend_position = legend_position,
    show_grid = show_grid,
    grid_color = grid_color,
    background = background
  )
  assert_vistool_theme(th)
  th
}

#' Validate a vistool theme object
#' @keywords internal
assert_vistool_theme = function(theme) {
  checkmate::assert_list(theme, names = "unique")
  if (!is.null(theme$palette)) checkmate::assert_choice(theme$palette, choices = c("viridis", "plasma", "grayscale"))
  if (!is.null(theme$text_size)) checkmate::assert_number(theme$text_size, lower = 1)
  if (!is.null(theme$theme)) checkmate::assert_choice(theme$theme, choices = c("minimal", "bw", "classic", "gray", "grey", "light", "dark", "void"))
  if (!is.null(theme$alpha)) checkmate::assert_number(theme$alpha, lower = 0, upper = 1)
  if (!is.null(theme$line_width)) checkmate::assert_number(theme$line_width, lower = 0)
  if (!is.null(theme$point_size)) checkmate::assert_number(theme$point_size, lower = 0)
  if (!is.null(theme$legend_position)) checkmate::assert_choice(theme$legend_position, choices = c("top", "right", "bottom", "left", "none"))
  if (!is.null(theme$show_grid)) checkmate::assert_flag(theme$show_grid)
  if (!is.null(theme$grid_color)) checkmate::assert_string(theme$grid_color)
  if (!is.null(theme$background)) checkmate::assert_string(theme$background)
  invisible(TRUE)
}

#' Merge two themes (override onto base)
#' @keywords internal
merge_theme = function(base, override) {
  if (is.null(override)) {
    return(base)
  }
  assert_vistool_theme(base)
  assert_vistool_theme(override)
  mlr3misc::insert_named(base, override)
}

#' Get/Set package default theme
#' @keywords internal
get_pkg_theme_default = function() {
  th = getOption("vistool.theme", NULL)
  if (is.null(th)) {
    th = vistool_theme()
  }
  th
}

#' @keywords internal
set_pkg_theme_default = function(theme) {
  assert_vistool_theme(theme)
  options(vistool.theme = merge_theme(vistool_theme(), theme))
  invisible(TRUE)
}
