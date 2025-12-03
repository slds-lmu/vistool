#' vistool theming utilities
#'
#' @description
#' Lightweight theme model and helpers to manage plotting style in a single place.
#' Can be used with vistool visualizers via `set_theme()` or added directly to
#' ggplot2 plots using the `+` operator.
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
#'
#' # Use with ggplot2
#' library(ggplot2)
#' ggplot(mtcars, aes(x = wt, y = mpg)) +
#'   geom_point() +
#'   vistool_theme()
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
  class(th) = c("vistool_theme", "list")
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

#' ggplot2 theme matching vistool defaults
#'
#' @param theme Optional vistool theme object. Falls back to the active
#'   `vistool_theme()` (global default) when `NULL`.
#' @return A [`ggplot2::theme`] object that can be composed via `+` or passed to
#'   `ggplot2::theme_set()`.
#' @examples
#' ggplot2::theme_set(theme_vistool())
#'
#' ggplot2::ggplot(mtcars, ggplot2::aes(wt, mpg)) +
#'   ggplot2::geom_point() +
#'   theme_vistool()
#' @export
theme_vistool = function(theme = NULL) {
  if (is.null(theme)) {
    theme = get_pkg_theme_default()
  }
  assert_vistool_theme(theme)

  theme_func = switch(theme$theme,
    "minimal" = ggplot2::theme_minimal,
    "bw" = ggplot2::theme_bw,
    "classic" = ggplot2::theme_classic,
    "gray" = ggplot2::theme_gray,
    "grey" = ggplot2::theme_grey,
    "light" = ggplot2::theme_light,
    "dark" = ggplot2::theme_dark,
    "void" = ggplot2::theme_void,
    ggplot2::theme_minimal
  )

  base_theme = theme_func(base_size = theme$text_size)

  title_size = theme$text_size + 2
  additions = ggplot2::theme(
    plot.title = ggplot2::element_text(size = title_size, hjust = 0.5),
    plot.background = ggplot2::element_rect(fill = theme$background, color = NA),
    panel.background = ggplot2::element_rect(fill = theme$background, color = NA),
    legend.position = theme$legend_position
  )

  if (!theme$show_grid) {
    additions = additions + ggplot2::theme(
      panel.grid.major = ggplot2::element_blank(),
      panel.grid.minor = ggplot2::element_blank()
    )
  } else if (!is.null(theme$grid_color)) {
    additions = additions + ggplot2::theme(
      panel.grid.major = ggplot2::element_line(color = theme$grid_color),
      panel.grid.minor = ggplot2::element_line(color = theme$grid_color, linewidth = 0.5)
    )
  }

  base_theme + additions
}

#' Add vistool theme to ggplot2
#'
#' @param object A vistool_theme object.
#' @param plot A ggplot object.
#' @param ... Additional arguments (unused).
#' @keywords internal
#' @exportS3Method ggplot2::ggplot_add
ggplot_add.vistool_theme = function(object, plot, ...) {
  plot + theme_vistool(object)
}
